%% Clear environment
close all;
clear;
clc;

%% Read data
traj = readtable('traj.csv');

%% Extract information
% Positions
x = traj.x;
y = traj.y;
t = traj.time;

% Measure Petri dish
xlims = [max(x), min(x)];
ylims = [max(y), min(y)];
r_0 = [mean(xlims), mean(ylims)]; % Center of the dish
x_r = x - r_0(1); y_r = y - r_0(2); % Recentered coordinates
R = max(max([abs(x_r), abs(y_r)])); % Radius of the dish

% Polar coordinates
r = sqrt(x_r.^2+y_r.^2);
th = atan2(y_r, x_r);

% Distance travelled
d = cumsum(r);

% Speeds
vx = gradient(x, t);
vy = gradient(y, t);
v = sqrt(vx.^2 + vy.^2);

% Accelerations
ax = gradient(vx, t);
ay = gradient(vy, t);
a = sqrt(ax.^2 + ay.^2);

%% Plot info
% Plot attributes
pos_col = 'k'; % Positions color
vel_col = 'r'; % Velocities color
acc_col = 'g'; % Accelerations color
pet_col = 'g'; % Petri dish color

subplot(4, 2, 1); % Positions in the Petri dish
scatter(x, y, 1, v); % Use v for color
hold on;
scatter(x(1), y(1), 10, 'o', 'red', 'filled');

% Plot Petri dish itself
scatter(r_0(1), r_0(2), 10, 'o', pet_col, 'filled'); % Center
s = linspace(0, 2*pi, 100); % Border
plot(r_0(1) + R.*cos(s), r_0(2) + R.*sin(s), 'color', pet_col);

axis('equal');
title('Positions (directional)');
xlabel('x'); ylabel('y');

subplot(4, 2, 2); % Total distance covered
plot(t, d, pos_col);
title('Distances (absolute)');
xlabel('t'); ylabel('d');

subplot(4, 2, 3); % Speeds as vectors
scatter(vx, vy, '.', vel_col);
axis('equal');
title('Speeds (directional)');
xlabel('x'); ylabel('y');

subplot(4, 2, 4); % Speeds as time series
plot(t, v, vel_col);
title('Speed (absolute)');
xlabel('t'); ylabel('v');

subplot(4, 2, 5); % Accelerations as vectors
scatter(ax, ay, '.', acc_col);
axis('equal');
title('Accelerations (directional)');
xlabel('x'); ylabel('y');

subplot(4, 2, 6); % Accelerations as time series
plot(t, a, acc_col);
title('Acceleration (absolute)');
xlabel('t'); ylabel('a');

subplot(4, 2, 7); % Distribution of radii
histogram(r, 50, 'Normalization', 'probability');
xlim([0, R]);
title('Radius distribution');

subplot(4, 2, 8); % Distribution of angles
histogram(th, 50, 'Normalization', 'probability');
title('Angle distribution');
xlim([-pi, pi]);
xticks([-pi, -pi/2, 0, pi/2, pi]);
xticklabels({'-180', '-90', '0', '90', '180'});

figure; % Density heat map
densityplot(x,y, 'nbins', [20, 20])
title('Density');
axis equal;

%% Print information
clc;
fprintf('Mean speed: %e \n ', mean(v));
fprintf('Mean acceleration: %e \n ', mean(a));