#include <stdio.h>
#include <stdlib.h>
#include <wiringPi.h>
#include <unistd.h>


// set pin numbers
#define LED 27
#define UP 28
#define DOWN 29

void songify(int song){

	printf("songify with: %d\n", song);

	pinMode(UP, INPUT);
	pinMode(DOWN, INPUT);
	pinMode(LED, OUTPUT);

	digitalWrite(LED, LOW);

	int up_val = LOW;
	int dn_val = LOW;
	int vol = fork();
	if(vol < 0){return;}
	if(vol == 0){
		//Volume Controller
		while(1){
			up_val = digitalRead(UP); 
			dn_val = digitalRead(DOWN);
			
			//printf("up: %d, down: %d\n", up_val, dn_val);
	
			if(up_val){
				printf("increase Volume\n");
				system("./vol +");
			}
			
			if(dn_val){
				printf("decrease volume\n");
				system("./vol -");
			}
		}
		return;
	}

	int player = fork();
	if(player < 0){
		return;
	}

	if(player == 0) {
		//I am child
		
		if(song == 0){
			// no light, light LED
			printf("Closing time, no song, only LED\n");
			digitalWrite(LED, HIGH);

		} else if(song == 1){
			// low light, evening hours
			printf("Evening hours, chill song\n");
			execlp("mpg321", "mpg321", "Game of Thrones.mp3", NULL);
		} else if(song == 2){
			// full light, active hours
			printf("Active hours, play PUMPIN' SONG!!\n");
			execlp("mpg321", "mpg321", "Rocky - Gonna Fly Now.mp3", NULL);
		}

		return;
	}

	sleep(15);
	digitalWrite(LED, LOW);
	kill(player, 9);	// 9 is signal for KILL
	kill(vol, 9);

	return;
}
