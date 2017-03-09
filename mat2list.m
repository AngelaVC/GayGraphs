function [list_start list_end node_start node_end] = mat2list( F_all, d_fr, Opin_all, numsteps )
% converts adjacency matrix to adjacency list with friend and family edges listed separately
% converts original opinion matrix to list
% where first column is the node number
% second is the opinion
% third is a marker that says gay or not (note this is already coded in opinion)
% all of this is so that data can be exported to R and read there as a social network
% only outputs start and end of trial -- should at some point do all timesteps

len = length(F_all(:,:,1));
listLen = len*len;
list_start = zeros(listLen,4);
list_end = zeros(listLen,4);

%% F1 is family edges
%% F2 is friend edges
F1= F_all(:,:,:)>d_fr;
F2= F_all(:,:,:)==d_fr;

% Creates list with friend and family edges separate
list_row=1;
for i=1:len
    for j=1:len
        list_start(list_row,1:4)=[i j F1(i,j,1) F2(i,j,1)];
        list_end(list_row,1:4)=[i j F1(i,j,numsteps) F2(i,j,numsteps)];
        list_row=list_row+1;
    end
end

node_start=zeros(len,3);
node_end=zeros(len,3);

% Creates list of nodes with opinion and gay attribut separate
gay=Opin_all(:,:)==1;
for i=1:len
    node_start(i,1:3)=[i Opin_all(i,1) gay(i,1)];
    node_end(i,1:3)=[i Opin_all(i,numsteps) gay(i,numsteps)];
end






