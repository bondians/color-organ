#include "Spectrum.h"
#include <math.h>
#include <stdlib.h>
#include <string.h>

struct spectrum_t {
    fftw_plan plan;
    
    size_t fft_in_size;
    double *fft_in;
    
    size_t fft_out_size;
    complex *fft_out;
    
    double bin_size;
    size_t unprocessed;
};

spectrum_t *spectrum_init(size_t fft_size, double sample_rate) {
    spectrum_t *s = malloc(sizeof(spectrum_t));
    
    s->fft_in_size = fft_size;
    s->fft_in  = fftw_malloc(s->fft_in_size * sizeof(double));

    s->fft_out_size = fft_size/2 + 1;
    s->fft_out = fftw_malloc(s->fft_out_size * sizeof(complex));
    
    s->plan = fftw_plan_dft_r2c_1d(s->fft_in_size, s->fft_in, s->fft_out, FFTW_MEASURE);
    
    for (int i = 0; i < s->fft_in_size; i++) {
        s->fft_in[i] = 0.0;
    }
    
    s->bin_size = sample_rate / (double) fft_size;
    s->unprocessed = fft_size;
    
    return s;
}

void spectrum_destroy(spectrum_t *s) {
    fftw_destroy_plan(s->plan);
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
        memcpy(s->fft_in + n, s->fft_in, n * sizeof(double));
    } else {
        n = s->fft_in_size;
    }
    
    for (size_t i = 0; i < n; i++) {
        s->fft_in[i] = samples[i];
    }
    
    s->unprocessed += n;
    if (s->unprocessed > s->fft_in_size) s->unprocessed = s->fft_in_size;
    
    return n;
}

complex *spectrum_get(spectrum_t *s) {
        if (s->unprocessed) {
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
