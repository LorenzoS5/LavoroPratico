pkg load io

% Data import from Excel

data = xlsread("MSCI-Indexes.xlsx");
rawTime = datenum(1900,1,1) + data(:, 1) - 2;
time = datestr(rawTime, 'mmm-yyyy');
ACWI = data(:, 2);
ACWIexUSA = data(:, 3);
EM = data(:, 4);
EMexCH = data(:, 5);
WORLD = data(:, 6);
indexes = [ACWI, ACWIexUSA, EM, EMexCH, WORLD];
variation = indexes ./ indexes(1, :);



% Plots
figure;
plot(rawTime, indexes, "linewidth", 3);
title ("Stock index price from January 2011 to December 2024");
datetick('x', 'mm/yyyy');
xlim([rawTime(1) rawTime(end)]);
legend({'ACWI', 'ACWIexUSA', 'EM', 'EMexCH', 'WORLD'});


figure;
plot(rawTime, variation, "linewidth", 3);
title ("Variation from January 2011 to December 2024");
datetick('x', 'mm/yyyy');
xlim([rawTime(1) rawTime(end)]);
legend({'ACWI', 'ACWIexUSA', 'EM', 'EMexCH', 'WORLD'});
