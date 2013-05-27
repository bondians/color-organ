#include <OpenAL/al.h>
#include <OpenAL/alc.h>
#include <stdint.h>
#include <stdio.h>

#define SAMPLE_RATE             44100
#define CAPTUREBUF_SIZE         SAMPLE_RATE

int main(int argc, char *argv[]) {
    ALCdevice *mic;
    int16_t buf[SAMPLE_RATE];
    
    mic = alcCaptureOpenDevice(NULL, SAMPLE_RATE, AL_FORMAT_MONO16, CAPTUREBUF_SIZE);
    if (alGetError() != AL_NO_ERROR) {
        return 0;
    }
    
    alcCaptureStart(mic);
    while (1) {
        // ask how many samples are captured
        ALint samples_available;
        alcGetIntegerv(mic, ALC_CAPTURE_SAMPLES, (ALCsizei)sizeof(ALint), &samples_available);
        
        // get that many from the capture buffer
        alcCaptureSamples(mic, (ALCvoid *)buf, samples_available);
        
        // print the first, this is just a test
        printf ("%d\n", buf[0]); 
    }
    alcCaptureStop(mic);
    alcCaptureCloseDevice(mic);
    
    return 0;
}