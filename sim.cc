#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>
#include "sim.h"
#include <bits/stdc++.h>
#include<sstream>

using namespace std;

char hexdec[100];
string binary;
string value;
uint32_t i;
/*  "argc" holds the number of command-line arguments.
    "argv[]" holds the arguments themselves.

    Example:
    ./sim 32 8192 4 262144 8 3 10 gcc_trace.txt
    argc = 9
    argv[0] = "./sim"
    argv[1] = "32"
    argv[2] = "8192"
    ... and so on
*/
typedef
struct {
bool valid,dirty;
uint32_t tag,rank;
}mem_block;


class cache{
  public: 
   uint32_t blocksize,l1_size,l1_assoc,l2_size,l2_assoc,pref_n,pref_m;
   uint32_t l1_sets,l1_index,l1_boff,l1_tag,l1_bbits,l1_indexbits;
   uint32_t l2_sets,l2_index,l2_boff,l2_tag,l2_bbits,l2_indexbits;
   uint32_t l1r,l1r_hit,l1r_miss,l1rb,l1w,l1wm;
   uint32_t l2r,l2r_hit,l2r_miss,l2w,l2wb,l2wm;
   uint32_t memtraffic;
   vector< vector<mem_block> > l1_cache;
   vector< vector<mem_block> > l2_cache;
   
   //ofstream fout;

   vector<int> l1_cur_size, l2_cur_size;

   cache(uint32_t blsz, uint32_t l1sz,uint32_t l1asc,uint32_t l2sz,uint32_t l2asc,uint32_t pre_n,uint32_t pre_m){
      blocksize=blsz;
      l1_size=l1sz;
      l1_assoc=l1asc;
      l2_size=l2sz;
      l2_assoc=l2asc;
      pref_n=pre_n;
      pref_m=pre_m;

	   l1r=l1r_hit=l1r_miss=l1rb=l1w=l1wm=l2r=l2r_hit=l2r_miss=l2w=l2wm=memtraffic=l2wb=0;
      
      
      //fout.open("Hello.txt", ios::out);
             
      if(l1_size != 0 && l1_assoc != 0)
      {
         l1_sets = l1_size/(l1_assoc*blocksize);
	l1_cur_size = vector<int> (l1_sets, 0);

         l1_indexbits = log2(l1_sets);
         l1_bbits = log2(blocksize);
         l1_tag = 32 - l1_indexbits - l1_bbits;
         
         l1_cache = vector< vector<mem_block> >(l1_sets);
         for(uint32_t i=0; i<l1_sets;i++)
            l1_cache[i] = vector<mem_block>(l1_assoc);


         for(uint32_t i=0;i<l1_sets;i++)
         {
            for(uint32_t j=0;j<l1_assoc;j++)
            {
               l1_cache[i][j].dirty = false;
               l1_cache[i][j].rank = j;
               l1_cache[i][j].tag = 0;
               l1_cache[i][j].valid = false;
            }
         }
      }

      if(l2_size!=0 && l2_assoc!=0)
      {
         l2_sets = l2_size/(l2_assoc*blocksize);
	      l2_cur_size = vector<int> (l2_sets, 0);

         l2_indexbits = log2(l2_sets);
         l2_bbits = log2(blocksize);
         l2_tag = 32 - l2_indexbits - l2_bbits;

         l2_cache = vector< vector<mem_block> > (l2_sets);
         for(uint32_t i=0;i<l2_sets;i++)
            l2_cache[i] = vector<mem_block>(l2_assoc);
         
         for(uint32_t i=0;i<l2_sets;i++)
         {
            for(uint32_t j=0;j<l2_assoc;j++)
            {
               l2_cache[i][j].dirty = false;
               l2_cache[i][j].rank = j;
               l2_cache[i][j].tag = 0;
               l2_cache[i][j].valid = false;
            }
         }
      }
   }

   //Function to return the block value of the address in the cache, -1 if not found.
   uint32_t parse(uint32_t index,uint32_t tag, vector< vector<mem_block> > &cac, uint32_t assoc)
   {
      ////fout  << "Finding in cache" << endl;
      for(i = 0; i < assoc; i++)
      {
         if(cac[index][i].tag == tag && cac[index][i].valid == 1)
         {
            return i;
         }
      }
      return -1;
   }

   //Function to update the lru of both L1 and L2 cache blocks.
   void update_lru(uint32_t index, uint32_t loc, uint32_t assoc, vector< vector<mem_block> > &l)
   {
     ////fout  << "Updating LRU" << endl;
      for(uint32_t i = 0; i < assoc; i++)
      {
         if(l[index][i].rank < l[index][loc].rank)
         {
            l[index][i].rank++;
         }
      }

      l[index][loc].rank = 0;
   }

   uint32_t max_rank(uint32_t index, uint32_t assoc, vector< vector<mem_block> > &l)
   {
      ////fout  << "Returning max rank" << endl;
      uint32_t max_rank = 0, loc = assoc;
      for(uint32_t i = 0; i < assoc; i++)
      {
         if(l[index][i].rank == assoc - 1)
         {
            // max_rank = l[index][i].rank;
            loc = i;
         }
      }

	//////fout  << "location in max rank = " << loc << endl;

      return loc;      

   }

   //Performs L2 read operation.
   void l2_read(uint32_t addr, char c)
   {
      ////fout  << "In L2 read" << endl;
	   if(c != 'e')
      	l2r = l2r + 1;

      uint32_t l2_index = ((1 << l2_indexbits)-1)&(addr>>l2_bbits);
      uint32_t l2_tag = addr>>(l2_bbits+l2_indexbits);
      uint32_t loc = parse(l2_index,l2_tag,l2_cache,l2_assoc);

      if(loc != -1)
      {
         ////fout  << "Found in L2" << endl;
         l2r_hit = l2r_hit + 1;
         
         if(c == 'e')
         {
	         l2_cache[l2_index][loc].dirty = 1;
            l2w += 1;
	      }
         ///else         
            update_lru(l2_index,loc,l2_assoc,l2_cache);
      }
      
      else
      {  
         ////fout  << "Not found in L2" << endl;
         l2r_miss = l2r_miss + 1;
         memtraffic = memtraffic + 1;

         if(c == 'e')
         {
            l2wm++;
         }

         write_in_l2(addr, c);

      } 

      //fout << "L2 ->" << endl;
      //printCache(l2_cache, l2_index, l2_assoc);
   }

   // Performs l1 read operation.
   void l1_read(uint32_t addr)
   {
	//fout  << "Address Computed: " << hex << addr << endl;
      ////fout  << "In L1 read " << endl;
      
      l1r = l1r + 1;  //l1 read counter increase by 1.

      uint32_t l1_index = ((1<<l1_indexbits)-1)&(addr>>l1_bbits);  // index seperation from address. 
      uint32_t l1_tag = addr >> (l1_bbits+l1_indexbits);  // Tag seperation using address.
      //fout  << "Tag for L1 = " << l1_tag << endl;
      
      uint32_t loc = parse(l1_index, l1_tag, l1_cache, l1_assoc);
      
      if(loc != -1)
      {
         l1r_hit = l1r_hit + 1;
         ////fout <<"Found in L1"<<endl;
         update_lru(l1_index, loc, l1_assoc, l1_cache);
      }
      else 
      {
         ////fout  << "Found in L1" << endl;
         l1r_miss = l1r_miss + 1;
	
	 write_in_l1(addr, 'r');

         if(l2_size != 0 && l2_assoc != 0)
            l2_read(addr, 'r');
	      
         if(l2_size == 0 || l2_assoc == 0)
         {
            memtraffic++;
         }

              }

      //fout << "L1 -> " << endl;
      //printCache(l1_cache, l1_index, l1_assoc);

   }

   bool check_eviction_in_l1(uint32_t index)
   {
	  ////fout  << "Checking eviction in L1" << endl;
      if(l1_cur_size[index] < l1_assoc)
      {
         return false;
      }
      
      return true;
   }

   void write_in_l1(uint32_t addr, char c)
   {
      ////fout  << "In write in L1" << endl;
      uint32_t index = ((1<<l1_indexbits)-1)&(addr>>l1_bbits);  // index seperation from address. 
      uint32_t tag = addr >> (l1_bbits+l1_indexbits);  // Tag seperation using address.
      
      bool is_eviction = check_eviction_in_l1(index);
      uint32_t l1_temp = max_rank(index, l1_assoc, l1_cache);

      if(is_eviction)
      {
         //fout  << "Evixtion in L1" << endl;
         if(l1_cache[index][l1_temp].dirty == 1)
         {
               		
            uint32_t evict_address = ( index << l1_bbits)| (l1_cache[index][l1_temp].tag << (l1_bbits + l1_indexbits) );

            l1rb = l1rb + 1;

            //fout  << "Dirty is 1" << endl;

            if(l2_size != 0 && l2_assoc != 0)
               l2_read(evict_address, 'e');
            else 
            {
               memtraffic++;
            }
         }
      }

      //fout  << "Writing new value in L1" << endl;
      l1_cache[index][l1_temp].tag = tag;
      l1_cache[index][l1_temp].valid = 1;
      l1_cache[index][l1_temp].dirty = (c == 'w');

      if(!is_eviction)
         l1_cur_size[index]++;

      update_lru(index, l1_temp, l1_assoc, l1_cache);

   }

   bool check_eviction_in_l2(uint32_t index)
   {
      //fout  << "Checking eviction in L2" << endl;
      if(l2_cur_size[index] < l2_assoc){
         return false;
      }
      return true;
   }

   void write_in_l2(uint32_t addr, char c)
   {
      //fout  << "In write in L2" << endl;
      uint32_t index = ( ( 1 << l2_indexbits ) - 1 ) & ( addr >> l2_bbits );  // index seperation from address.
      uint32_t tag = addr>>(l2_bbits+l2_indexbits);  // Tag seperation using address.
 
      bool is_eviction = check_eviction_in_l2(index);
      uint32_t l2_temp = max_rank(index, l2_assoc, l2_cache);

      if(is_eviction)
      { 
         //fout  << "Eviction in L2" << endl;
         if(l2_cache[index][l2_temp].dirty == 1)
         {
            memtraffic += 1;
            l2wb++;
         }      
      }

      //fout  << "Writing value in L2" << endl;
      l2_cache[index][l2_temp].tag = tag;
      l2_cache[index][l2_temp].valid = 1;
      l2_cache[index][l2_temp].dirty = (c == 'e');

      if(!is_eviction)
         l2_cur_size[index]++;
      
      if(c != 'e')
         update_lru(index, l2_temp, l2_assoc, l2_cache);
   }

   void l1_write(uint32_t addr)
   { 
      //fout  << "In L1 write" << endl;
      l1w = l1w + 1;

      uint32_t l1_index = ((1<<l1_indexbits)-1)&(addr>>l1_bbits);  // index seperation from address. 
      uint32_t l1_tag = addr>>(l1_bbits+l1_indexbits);  // Tag seperation using address.
      uint32_t loc = parse(l1_index,l1_tag,l1_cache,l1_assoc);
      
	//fout << "Address Computed "<< hex << addr<<endl;  
      if(loc != -1)
      {  
         //fout  << "Found in L1" << endl;
         l1_cache[l1_index][loc].dirty = 1;
         update_lru(l1_index, loc, l1_assoc, l1_cache);
      }
      else
      {
         //fout  << "Not found in L1" << endl;
         l1wm = l1wm + 1;

	    write_in_l1(addr, 'w');
         
         if(l2_size != 0 && l2_assoc != 0)
            l2_read(addr, 'w');

	      if(l2_size == 0 || l2_assoc == 0){
            memtraffic++;
         }

           }

      //fout << "L1 -> " << endl;
      //printCache(l1_cache, l1_index, l1_assoc);

   }

   void printCache(vector<vector<mem_block> > &l, uint32_t index, int assoc)
   {
      

   }
};

int main (uint32_t argc, char *argv[]) {
   
 
   FILE *fp;			// File pointer.
   char *trace_file;		// This variable holds the trace file name.
   cache_params_t params;	// Look at the sim.h header file for the definition of struct cache_params_t.
   char rw;			// This variable holds the request's type (read or write) obtained from the trace.
   uint32_t addr;		// This variable holds the request's address obtained from the trace.
				// The header file <inttypes.h> above defines signed and unsigned integers of various sizes in a machine-agnostic way.  "uint32_t" is an unsigned integer of 32 bits.

   // Exit with an error if the number of command-line arguments is incorrect.
   if (argc != 9) {
      printf("Error: Expected 8 command-line arguments but was provided %d.\n", (argc - 1));
      exit(EXIT_FAILURE);
   }
    
   // "atoi()" (included by <stdlib.h>) converts a string (char *) to an integer (int).
   params.BLOCKSIZE = (uint32_t) atoi(argv[1]);
   params.L1_SIZE   = (uint32_t) atoi(argv[2]);
   params.L1_ASSOC  = (uint32_t) atoi(argv[3]);
   params.L2_SIZE   = (uint32_t) atoi(argv[4]);
   params.L2_ASSOC  = (uint32_t) atoi(argv[5]);
   params.PREF_N    = (uint32_t) atoi(argv[6]);
   params.PREF_M    = (uint32_t) atoi(argv[7]);
   trace_file       = argv[8];

   // Open the trace file for reading.
   fp = fopen(trace_file, "r");
   if (fp == (FILE *) NULL) {
      // Exit with an error if file open failed.
      printf("Error: Unable to open file %s\n", trace_file);
      exit(EXIT_FAILURE);
   }
    
   // Pruint32_t simulator configuration.
   printf("===== Simulator configuration =====\n");
   printf("BLOCKSIZE:  %u\n", params.BLOCKSIZE);
   printf("L1_SIZE:    %u\n", params.L1_SIZE);
   printf("L1_ASSOC:   %u\n", params.L1_ASSOC);
   printf("L2_SIZE:    %u\n", params.L2_SIZE);
   printf("L2_ASSOC:   %u\n", params.L2_ASSOC);
   printf("PREF_N:     %u\n", params.PREF_N);
   printf("PREF_M:     %u\n", params.PREF_M);
   printf("trace_file: %s\n", trace_file);
   printf("\n");

   cache c (params.BLOCKSIZE,  params.L1_SIZE, params.L1_ASSOC, params.L2_SIZE, params.L2_ASSOC, params.PREF_N, params.PREF_M );
   
   // cout << endl << "L1 ->" << endl;
   // c.printCache(c.l1_cache, c.l1_sets, c.l1_assoc);

   // cout << endl << "L2 ->" << endl;
   // c.printCache(c.l2_cache, c.l2_sets, c.l2_assoc);

   // Read requests from the trace file and echo them back.
   while (fscanf(fp, "%c %x\n", &rw, &addr) == 2) {	// Stay in the loop if fscanf() successfully parsed two tokens as specified.
      
      if (rw == 'r')
      {
         //c.fout << "New read request " << endl;
        // printf("r %x\n", addr);
         c.l1_read(addr);
      }
      else if (rw == 'w'){
         //printf("w %x\n", addr);
         c.l1_write(addr);
         }
      else {
        // printf("Error: Unknown request type %c.\n", rw);
	 exit(EXIT_FAILURE);
      }
	} 
	
  
   cout<<"===== L1 contents ====="<<endl; 
   map<int, pair <int,bool> > temp;
   if(c.l1_size!=0)
   {
      for(int i=0;i<c.l1_sets;i++)
      {
         temp.clear();
         for(int j=0;j<c.l1_assoc;j++)
         {
            if(c.l1_cache[i][j].valid != 0)
            {
               temp.insert(make_pair(c.l1_cache[i][j].rank,make_pair(c.l1_cache[i][j].tag,c.l1_cache[i][j].dirty)));
            }
            //cout<<" c "<<cache.l1_cache[i][j].rank<<cache.l1_cache[i][j].tag<<endl;
         }
         cout<<"set "<<dec<<i<<":";
         map<int, pair <int,bool> >::iterator k;
         for(k = temp.begin(); k != temp.end(); k++)
         {
            cout<<" "<<hex<<(k->second).first<<" ";
            if((k->second).second==1)cout<<"D ";
            else cout<<"  ";
         }
         cout <<"\n";
      }
   }

   if(c.l2_size != 0)
   {
      cout << endl;
      cout<<"===== L2 contents =====" << endl; 
      for(int i = 0; i < c.l2_sets; i++)
      {
         map<int, pair <int, bool> > temp;
         for(int  j = 0; j < c.l2_assoc; j++)
         {
            if(c.l2_cache[i][j].valid != 0)
            {
               temp.insert( make_pair ( c.l2_cache[i][j].rank, make_pair( c.l2_cache[i][j].tag, c.l2_cache[i][j].dirty)));
            }
         }
         cout<<"set "<<dec<<i<<":";
         map<int, pair <int, bool> >::iterator k;
         for( k= temp.begin(); k != temp.end(); k++)
         {
            cout << " " << hex << k->second.first << " ";
            
            if(k->second.second == 1)
               cout<<"D ";
            else cout<<"  ";
         }
         cout <<"\n";
      }
   }
    

   printf("\n");
   cout<<"===== Measurements ====="<<endl; 
	cout << dec << "a. L1 reads:                   " << c.l1r << endl;
   cout << "b. L1 read misses:             " << c.l1r_miss << endl;
	cout << "c. L1 writes:                  " << c.l1w << endl;
   cout << "d. L1 write misses:            " << c.l1wm<< endl;
   cout << "e. L1 miss rate:               " << setprecision(4) << fixed << (double) (c.l1r_miss + c.l1wm)/(c.l1r + c.l1w) << endl;
   cout << "f. L1 writebacks:              " << c.l1rb<< endl;
   cout << "g. L1 prefetches:              " << 0 << endl; // number of L1 prefetches
	cout << "h. L2 reads (demand):          " << c.l2r << endl;
   cout << "i. L2 read misses (demand):    " << c.l2r_miss << endl;
   cout << "j. L2 reads (prefetch):        " << 0 << endl; //number of L2 reads that originated from L1 prefetches:
   cout << "k. L2 read misses (prefetch):  " << 0 << endl; //number of L2 read misses that originated from L1 prefetches:
	cout << "l. L2 writes:                  " << c.l2w << endl;
   cout << "m. L2 write misses:            " << c.l2wm << endl;
   if (isnan((double)(c.l2r_miss)/(c.l2r)))
   {
   cout<<"n. L2 miss rate:               "<<0.0000<<endl;
   }
   else
   {
   cout<<"n. L2 miss rate:               "<<setprecision(4)<<fixed<<(double)(c.l2r_miss)/(c.l2r)<<endl; //L2 miss rate:
   }
   // cout << "n. L2 miss rate: " << (c.l2r_miss)/(c.l2r) << endl;
   cout << "o. L2 writebacks:              " << c.l2wb << endl;
   cout << "p. L2 prefetches:              " << 0 << endl;
	cout << "q. memory traffic:             " << c.memtraffic << endl;

   return(0);
}



