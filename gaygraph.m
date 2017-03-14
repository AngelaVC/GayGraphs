% Implementation of gay opinion change on a random graph

clear all

%------------
%************
% Set all parameters
%************
%------------

%number of nodes
n = 50;
%edge probability
p = 0.3;
%number of time steps to run
numsteps=5000;

%set proportion of nodes that are "family"
pf=0;
% Set threshold for breaking edge (higher is harder to break)
d_fa=.9; % "family" threshold
d_fr=.2; % "friend" threshold

% proportion of nodes that are queer
pg=.1;

%---------
% probability of adjustment (doesn't always happen, random unless set to 0
% controls rate of change of the network
w=0;

%---------
% convergence parameter, how fast opinions approach each other
% if u=.5, every opinion adjustment results in average
u=.25;

%---------
% minimum and maximum opinions, opinions are uniformly distributed in range
opinMin=.5;
opinMax=.99999999999;


% Variables for naming files
% note that name_code can be set to help locate files
name_code='for-talk';
c=clock;
IDtag=strcat(num2str(c(1)),'-', num2str(c(2)),'-', num2str(c(3)), '-', num2str(c(4)),'-',num2str(c(5)));

%----------
% Initialize matrices that will be filled each time step and set max
% timesteps
%----------

E_all=zeros(n,n,numsteps);
F_all=zeros(n,n,numsteps);
Opin_all=zeros(n,numsteps);

%----------
%matrix of undirected edges placed with probability p, 
%generate full random graph, take only upper trangle, transpose, and add
%----------
E = rand(n,n) < p;
E = triu(E,1);
E = E + E'; 

%-----------
%matrix of possible Friend/Family designation
%Friend is +1, Family is -1
%Note this is an edge feature, entries only for edges present in E
%-----------

F=rand(n,n)<pf;
F=triu(F,1);
F=d_fa.*(F+F');
F(F==0)=d_fr;
F=F.*E;


%-----------
%Designate gay nodes
%-----------
G=rand(n,1)<pg;

%-----------
% Make matrix of original opinions, uniform on a range between
% opinMin>=0 and opinMax<1 higher numbers more accepting
% opinMin=0, opinMax=.99999999 gives fullest possible spectrum
% Alternate choice would be to used normal distribution
%-----------
Opin = opinMin + (opinMax-opinMin).*rand(n,1);

%replace gay nodes with 1
j=find(G==1);
Opin(j)=1;

%----------------
%Put original edges, friend/fam designation, and Opin into first position
%of time series
%----------------

E_all(:,:,1)=E;
F_all(:,:,1)=F;
Opin_all(:,1)=Opin;

%-------------
% Iterate through adjustment algorithm for numsteps-1 more steps
%-------------

for t=2:numsteps
[E F Opin]=opinadj(E,F,Opin,n,d_fr,w,u);
E_all(:,:,t)=E;
F_all(:,:,t)=F;
Opin_all(:,t)=Opin;
end

%-------------
% Plot histogram of opinions
%-------------
figure
hist(Opin_all(:,1),min(0):.1:max(1))
title('Initial Opinion Histogram')
figfile_start=strcat(name_code,'-histfig-', IDtag, '-start.jpg');
print(figfile_start,'-djpg')

figure
hist(Opin_all(:,end),min(0):.1:max(1))
title(['Opinion Histogram after ',num2str(numsteps-1),' iterations'])
figfile_end=strcat(name_code,'-histfig-',IDtag, '-end.jpg');
print(figfile_end,'-djpg')

%-------------
% Output edges and nodes for reading into R
%-------------

 [Ls Le Ns Ne]=mat2list(F_all,d_fr,Opin_all,numsteps);

 datafile=strcat(name_code,'-data-', IDtag)

 save('-ascii',strcat(datafile, '-edges-start.txt'),'Ls')
 save('-ascii',strcat(datafile, '-edges-end.txt'),'Le')
 save('-ascii',strcat(datafile, '-nodes-start.txt'),'Ns')
 save('-ascii',strcat(datafile, '-nodes-end.txt'),'Ne')

%-------------
% Output file with parameters
%-------------
%%% problem -- this isn't actually opening and outputting!!!

%%save('-ascii',strcat(name_code,'-',IDtag,'-params.txt'),'n','p','pf')
%% strcat(name_code,'-',IDtag,'-params.txt')
%fid=fopen('delete-me','w');
%fprintf(fid,'Nodes')
%%% =%f, Edge prob=%f, Family prob=%f, Family threshold=%f, Friend Threshold=%f, Queer prob=%f',n,p,pf,d_fa,d_fr,pg);
%fclose(fid);
%

