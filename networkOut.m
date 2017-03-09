%%
%% This runs a trial and outputs for reading into R
%%

 gaygraph;
 [Ls Le Ns Ne]=mat2list(F_all,d_fr,Opin_all,numsteps);

 save 'edgesForTalkStart.txt' Ls -ascii
 save 'edgesForTalkEnd.txt' Le -ascii

 save 'nodesForTalkStart.txt' Ns -ascii
 save 'nodesForTalkEnd.txt' Ne -ascii



