
# if running on an M1 system (ie recent mac hardware)
export DOCKER_DEFAULT_PLATFORM=linux/amd64

biolockj --docker --blj_proj ../results anigma_stress.config
