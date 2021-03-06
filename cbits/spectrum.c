#include "spectrum.h"
#include <math.h>
#include <stdlib.h>
#include <string.h>

struct spectrum_t {
    fftw_plan plan;
    
    size_t fft_in_size;
    double *window;
    double *sample_buf;
    double *fft_in;
    
    size_t fft_out_size;
    complex *fft_out;
    
    double bin_size;
    size_t unprocessed;
};

spectrum_t *spectrum_init(double sample_rate, size_t fft_size, double *window) {
    spectrum_t *s = malloc(sizeof(spectrum_t));
    
    s->fft_in_size = fft_size;
    
    s->fft_in  = fftw_malloc(s->fft_in_size * sizeof(double));

    if (window) {
        s->sample_buf = fftw_malloc(s->fft_in_size * sizeof(double));
        s->window = fftw_malloc(s->fft_in_size * sizeof(double));
        memcpy(s->window, window, s->fft_in_size * sizeof(double));
    } else {
        s->sample_buf = s->fft_in;
        s->window = NULL;
    }
    
    for (int i = 0; i < s->fft_in_size; i++) {
        s->sample_buf[i] = 0.0;
    }
    
    s->fft_out_size = fft_size/2 + 1;
    s->fft_out = fftw_malloc(s->fft_out_size * sizeof(complex));
    
    s->plan = fftw_plan_dft_r2c_1d(s->fft_in_size, s->fft_in, s->fft_out, FFTW_MEASURE);
        
    s->bin_size = sample_rate / (double) fft_size;
    s->unprocessed = fft_size;
    
    return s;
}

void spectrum_cleanup(spectrum_t *s) {
    fftw_destroy_plan(s->plan);
    if (s->window) {
        fftw_free(s->window);
        fftw_free(s->sample_buf);
    }
    fftw_free(s->fft_in);
    fftw_free(s->fft_out);
    
    free(s);
}

int spectrum_bin(spectrum_t *s, double freq) {
    return round(freq / s->bin_size);
}
double spectrum_freq(spectrum_t *s, int bin) {
    return bin * s->bin_size;
}

size_t spectrum_feed(spectrum_t *s, size_t n, int16_t *samples) {
    if (n <= 0) return 0;
    
    if (n < s->fft_in_size) {
        memmove(s->sample_buf + n, s->sample_buf, n * sizeof(double));
    } else {
        n = s->fft_in_size;
    }
    
    for (size_t i = 0; i < n; i++) {
        s->sample_buf[i] = samples[i];
    }
    
    s->unprocessed += n;
    if (s->unprocessed > s->fft_in_size) s->unprocessed = s->fft_in_size;
    
    return n;
}

complex *spectrum_get(spectrum_t *s) {
        if (s->unprocessed) {
            if (s->window) {
                int n = s->fft_in_size;
                double *sBuf = s->sample_buf;
                double *win  = s->window;
                double *out = s->fft_in;
                
                for (int i = 0; i < n; i++) {
                    out[i] = sBuf[i] * win[i];
                }
            }
            
            fftw_execute(s->plan);
            s->unprocessed = 0;
        }
        
        return s->fft_out;
}

double spectrum_get_mag(spectrum_t *s, double freq) {
    int bin = spectrum_bin(s, freq);
    if (bin < 0 || bin >= s->fft_out_size) return 0.0;
    
    return cabs(spectrum_get(s)[bin]);
}
