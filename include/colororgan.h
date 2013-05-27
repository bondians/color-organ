#ifndef ___n_audio_h__
#define ___n_audio_h__

#include <unistd.h>
#include "spectrum.h"

typedef struct color_organ_t color_organ_t;

color_organ_t *color_organ_init(double sample_rate, size_t capturebuf_size);
void color_organ_cleanup(color_organ_t *c);

void color_organ_tick(color_organ_t *c, spectrum_t *s);

#endif /* ___n_audio_h__ */
