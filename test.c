#include <complex.h>
#include <fftw3.h>
#include <math.h>
#include <OpenAL/al.h>
#include <OpenAL/alc.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

#define SAMPLE_RATE             44100

#define FFT_BLOCK_IN_SIZE       16384
#define FFT_BLOCK_OUT_SIZE      (FFT_BLOCK_IN_SIZE / 2 + 1)

#define FFT_CHUNK_SIZE          441
#define CAPTUREBUF_SIZE         (SAMPLE_RATE/25)

double freq(int bin) {
    return bin * ((double) SAMPLE_RATE / (double) FFT_BLOCK_IN_SIZE);
}

int bin(double freq) {
    return round(freq * ((double) FFT_BLOCK_IN_SIZE / (double) SAMPLE_RATE));
}

void process_chunk(fftw_plan plan, double *fft_in, fftw_complex *fft_out, const int16_t *samples) {
    memcpy(fft_in + FFT_CHUNK_SIZE, fft_in, FFT_CHUNK_SIZE * sizeof(double));
    
    for (int i = 0; i < FFT_CHUNK_SIZE; i++) {
        fft_in[i] = samples[i];
    }
    
    fftw_execute(plan);
    
    int a = bin(440);
    printf("%g: %g\n", freq(a), log(cabs(fft_out[a])));
}

int main(int argc, char *argv[]) {
    double *fft_in = fftw_malloc(sizeof(double) * FFT_BLOCK_IN_SIZE);
    fftw_complex *fft_out = fftw_malloc(sizeof(fftw_complex) * FFT_BLOCK_IN_SIZE);
    fftw_plan plan = fftw_plan_dft_r2c_1d(FFT_BLOCK_IN_SIZE, fft_in, fft_out, FFTW_MEASURE);
    ALCdevice *mic;
    int16_t buf[FFT_CHUNK_SIZE];
    
    for (int i = 0; i < FFT_BLOCK_IN_SIZE; i++) {
        fft_in[i] = 0.0;
    }
    
    mic = alcCaptureOpenDevice(NULL, SAMPLE_RATE, AL_FORMAT_MONO16, CAPTUREBUF_SIZE);
    if (alGetError() != AL_NO_ERROR) {
        return 0;
    }
    
    alcCaptureStart(mic);
    while (1) {
        ALint samples_available;
        alcGetIntegerv(mic, ALC_CAPTURE_SAMPLES, (ALCsizei)sizeof(ALint), &samples_available);
        
        if (samples_available >= FFT_CHUNK_SIZE) {
            alcCaptureSamples(mic, (ALCvoid *)buf, FFT_CHUNK_SIZE);
            
            process_chunk(plan, fft_in, fft_out, buf);
        }
    }
    alcCaptureStop(mic);
    alcCaptureCloseDevice(mic);
    
    fftw_destroy_plan(plan);
    fftw_free(fft_in);
    fftw_free(fft_out);
    
    return 0;
}