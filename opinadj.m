%Function for opinion convergence, called by gaygraph.m

function [E F Opin]=opinadj(E,F,Opin,n,d_fr,w,u)

% Randomly select node, node must have connections:
connections=[];
while isempty(connections)
      i=floor(rand()*n)+1;
      connections=find(E(i,:)==1);
end

% Randomly select a connected node:
j=connections(floor(rand()*length(connections))+1);


% Generate random number r
r=rand();

% Take this step only on some iterations, controls rate of convergence
if r>w 
    % take absolute difference of opinions  
    O_diff=abs(Opin(i)-Opin(j)); 
    
    % If opinions are closer than "d" they converge
    if O_diff<F(i,j)
        if (Opin(i)==1)
            Opin(j)=Opin(j)+(u*O_diff);
        elseif (Opin(j)==1)
            Opin(i)=Opin(i)+(u*O_diff);    
        elseif (Opin(i)<1) && (Opin(j)<1) && (Opin(i)<Opin(j))
            Opin(i)=Opin(i)+(u*O_diff);
            Opin(j)=Opin(j)-(u*O_diff);
        else
            Opin(i)=Opin(i)-(u*O_diff);
            Opin(j)=Opin(j)+(u*O_diff);
        end

    % If opinions farther than d, edge is broken, and new friend edge is placed   
    else
        % Break edge, in both Edge and Friend/Family graph
        E(i,j)=0;
        E(j,i)=0;
        F(i,j)=0;
        F(j,i)=0;
        % Make list of non-connected nodes, eliminate self node,
        % randomly select one to be new connection
        j_u=find(E(i,:)==0);
        index=find(j_u==i);
        j_u(index)=[];
        j_n=j_u(floor(rand()*length(j_u))+1);

        % Place new edge in edge graph and designate a friend edge in F
        % (don't replace family edges with family once they are broken)
        E(i,j_n)=1;
        E(j_n,i)=1;
        F(i,j_n)=d_fr;
        F(j_n,i)=d_fr;
    end
end
