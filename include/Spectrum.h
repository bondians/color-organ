#ifndef ___n_Spectrum_h__
#define ___n_Spectrum_h__

#include <complex.h>
#include <fftw3.h>
#include <stdint.h>
#include <unistd.h>

typedef struct spectrum_t spectrum_t;

spectrum_t *spectrum_init(size_t fft_size, double sample_rate);
void spectrum_cleanup(spectrum_t *s);

int spectrum_bin(spectrum_t *s, double freq);
double spectrum_freq(spectrum_t *s, int bin);

size_t spectrum_feed(spectrum_t *s, size_t n, int16_t *samples);
complex *spectrum_get(spectrum_t *s);

double spectrum_get_mag(spectrum_t *s, double freq);

#endif /* ___n_Spectrum_h__ */
