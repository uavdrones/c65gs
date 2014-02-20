/*
  LOAD kickstart ROM into place

Copyright (C) 2014 Paul Gardner-Stephen
Portions Copyright (C) 2013 Serval Project Inc.
 
This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.
 
This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.
 
You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
*/

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <termios.h>
#include <time.h>
#include <strings.h>
#include <string.h>
#include <ctype.h>
#include <sys/time.h>
#include <errno.h>

int process_char(unsigned char c,int live);

int slow_write(int fd,char *d,int l)
{
  // UART is at 230400bps, but reading commands has no FIFO, and echos
  // characters back, meaning we need a 1 char gap between successive
  // characters.  This means >=1/23040sec delay. We'll allow roughly
  // double that at 100usec.
  // printf("Writing [%s]\n",d);
  int i;
  for(i=0;i<l;i++)
    {
      usleep(100);
      write(fd,&d[i],1);
    }
  return 0;
}

int fd=-1;
int state=99;
int name_len,name_lo,name_hi,name_addr=-1;
char filename[17];
FILE *f=NULL;
char *search_path=".";

unsigned long long gettime_ms()
{
  struct timeval nowtv;
  // If gettimeofday() fails or returns an invalid value, all else is lost!
  if (gettimeofday(&nowtv, NULL) == -1)
    perror("gettimeofday");
  return nowtv.tv_sec * 1000LL + nowtv.tv_usec / 1000;
}

int main(int argc,char **argv)
{
  errno=0;
  fd=open(argv[1],O_RDWR);
  if (fd==-1) perror("open");
  fcntl(fd,F_SETFL,fcntl(fd, F_GETFL, NULL)|O_NONBLOCK);
  struct termios t;
  if (cfsetospeed(&t, B230400)) perror("Failed to set output baud rate");
  if (cfsetispeed(&t, B230400)) perror("Failed to set input baud rate");
  t.c_cflag &= ~PARENB;
  t.c_cflag &= ~CSTOPB;
  t.c_cflag &= ~CSIZE;
  t.c_cflag &= ~CRTSCTS;
  t.c_cflag |= CS8 | CLOCAL;
  t.c_lflag &= ~(ICANON | ISIG | IEXTEN | ECHO | ECHOE);
  t.c_iflag &= ~(BRKINT | ICRNL | IGNBRK | IGNCR | INLCR |
                 INPCK | ISTRIP | IXON | IXOFF | IXANY | PARMRK);
  t.c_oflag &= ~OPOST;
  if (tcsetattr(fd, TCSANOW, &t)) perror("Failed to set terminal parameters");

  FILE *f=fopen(argv[2]? argv[2] : "kickstart65gs.bin","r");
  if (f==NULL) {
    perror("File not found.");
    exit(-1);
  } else {
    unsigned long long load_addr = 0xfffe000;
    printf("Load address is $%08llx\n",load_addr);
    slow_write(fd,"\n",1);
    usleep(10000);
    unsigned char buf[1024];
    int b=fread(buf,1,1024,f);
    while(b>0) {
      int i;
      int n;
      for(i=0;i<b;i+=16) {
	if ((i+16)>b) n=b-i; else n=16;
	char cmd[64];
	printf("Read to $%04llx\r",load_addr);
	fflush(stdout);
	// XXX - writes 16 bytes even if there are less bytes ready.
	sprintf(cmd,"s%llx %02x %02x %02x %02x %02x %02x %02x %02x %02x %02x %02x %02x %02x %02x %02x %02x\r",
		load_addr,
		buf[i+0],buf[i+1],buf[i+2],buf[i+3],buf[i+4],buf[i+5],buf[i+6],buf[i+7],
		buf[i+8],buf[i+9],buf[i+10],buf[i+11],buf[i+12],buf[i+13],buf[i+14],buf[i+15]);
	
	slow_write(fd,cmd,strlen(cmd));
	usleep(2000);
	load_addr+=n;	
	{
	  unsigned char read_buff[1024];
	  read(fd,read_buff,1024);
	}
      }
      b=fread(buf,1,1024,f);
    }
    fclose(f); f=NULL;
    printf("\n");
  }
  // loaded ok.
  return 0;
}
