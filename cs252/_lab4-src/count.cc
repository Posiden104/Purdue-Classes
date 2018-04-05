
#include <pthread.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/time.h>

int count;

void increment(int ntimes )
{
	for ( int i = 0; i < ntimes; i++ ) {
		int c;

		c = count;
		c = c + 1;

		count = c;
	}
}

int main( int argc, char ** argv )
{
	//struct timeval stop, start;
	//gettimeofday(&start, NULL);

	int n = 10000000;
	pthread_t t1, t2;
        pthread_attr_t attr;

        pthread_attr_init( &attr );
        pthread_attr_setscope(&attr, PTHREAD_SCOPE_SYSTEM);

	printf("Start Test. Final count should be %d\n", 2 * n );

	// Create threads
	pthread_create( &t1, &attr, (void * (*)(void *)) increment, 
			(void *) n);

	pthread_create( &t2, &attr, (void * (*)(void *)) increment, 
			(void *) n);

	// Wait until threads are done
	pthread_join( t1, NULL );
	pthread_join( t2, NULL );

	if ( count != 2 * n ) {
		printf("\n****** Error. Final count is %d\n", count );
		printf("****** It should be %d\n", 2 * n );
	}
	else {
		printf("\n>>>>>> O.K. Final count is %d\n", count );
	}
	
	//gettimeofday(&stop, NULL);
	//long diff = (stop.tv_sec * 1000 + stop.tv_usec / 1000) - (start.tv_sec * 1000 + start.tv_usec / 1000);
	//printf("Execution time: %lu milliseconds\n", diff);
}


