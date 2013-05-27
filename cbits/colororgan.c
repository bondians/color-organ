#include "colororgan.h"
#include <OpenAL/al.h>
#include <OpenAL/alc.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include "spectrum.h"

struct color_organ_t {
    ALCdevice *mic;
    size_t capturebuf_size;
    int16_t *capture_buf;
};

color_organ_t *color_organ_init(double sample_rate, size_t capturebuf_size) {
    color_organ_t *c = malloc(sizeof(color_organ_t));
    
    c->mic = alcCaptureOpenDevice(NULL, (ALCuint) sample_rate, AL_FORMAT_MONO16, (ALCsizei) capturebuf_size);
    if (alGetError() != AL_NO_ERROR) {
        return NULL;
    }
    
    c->capturebuf_size = capturebuf_size;
    c->capture_buf = malloc(capturebuf_size * sizeof(int16_t));
    
    alcCaptureStart(c->mic);
    
    return c;
}

void color_organ_cleanup(color_organ_t *c) {
    alcCaptureStop(c->mic);
    alcCaptureCloseDevice(c->mic);
    free(c->capture_buf);
    free(c);
}

int color_organ_tick(color_organ_t *c, spectrum_t *s) {
    ALint samples_available;
    do {
        alcGetIntegerv(c->mic, ALC_CAPTURE_SAMPLES, (ALCsizei)sizeof(ALint), &samples_available);
    } while (!samples_available);
    
    if (samples_available) {
        alcCaptureSamples(c->mic, (ALCvoid *) c->capture_buf, samples_available);
        
        spectrum_feed(s, samples_available, c->capture_buf);
    }
    
    return samples_available;
}
