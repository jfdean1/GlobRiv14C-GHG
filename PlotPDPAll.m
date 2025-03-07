clc,clear

LargeVar={'pre_mm_uyr','Mean annual precipitation (mm)';
    'ele_mt_uav','Mean elevation (m a.s.l)';
    'tmp_dc_uyr','Mean annual temperature (°C×10)';%°C×10
    'kar_pc_use','Karst area extent (%)';
    'for_pc_use','Forest cover extent (%)'};

SmallVar={'ele_mt_cav','Mean elevation (m a.s.l)';
    'soc_th_cav','Organic carbon content (t ha^-^1)';
    'snd_pc_cav','Soil sand fraction (%)';
    'tmp_dc_cyr','Mean annual temperature (°C×10)'};%°C×10


%%
%Small catchments
figure('Renderer', 'painters', 'Position', [10 10 600 900])
for j=1:size(SmallVar,1)
    Data=[];
    Data=readtable(['Smallpdp_RR_',SmallVar{j,1},'.csv']);
    
    subplot(3,2,j)
    hold on

    Fdata_all = cell(10,1); 
    x_min = inf;
    x_max = -inf;

   
    for i = 1:10
        Fdata = table2array(Data(table2array(Data(:,4)) == i, :));
        Fdata_all{i} = Fdata;

       
        x_min = min(x_min, min(Fdata(:,2)));
        x_max = max(x_max, max(Fdata(:,2)));
    end

   
    x_common = linspace(x_min, x_max, 100); 

   
    y_matrix = zeros(length(x_common), 10); 

    for i = 1:10
        x_i = Fdata_all{i}(:,2);
        y_i = Fdata_all{i}(:,3);

        
        y_interp = interp1(x_i, y_i, x_common, 'linear', 'extrap');
        y_matrix(:, i) = y_interp;
    end

    y_min = min(y_matrix, [], 2);  
    y_max = max(y_matrix, [], 2);  
    y_mean = mean(y_matrix, 2);    

    
    fill([x_common, fliplr(x_common)], [y_min; flipud(y_max)]', 'b', 'FaceAlpha', 0.2, 'EdgeColor', 'none');

    
    plot(x_common, y_mean, 'b', 'LineWidth', 2)

    ylabel('F^{14}C_{atm}')
    xlabel(SmallVar{j,2})
    set(gca, 'fontsize', 12)
    box on
    

    hold off


end
legend('Range from 10-fold cross-validation runs','Median of 10 runs')

% print('-r1200','-djpeg',['ALSmallPdP.jpg'])
% close all

%%
%large catchments

figure('Renderer', 'painters', 'Position', [10 10 600 900])
for j=1:size(LargeVar,1)
    Data=[];
    Data=readtable(['Largepdp_RR_',LargeVar{j,1},'.csv']);
    
    subplot(3,2,j)
    hold on

    Fdata_all = cell(10,1); 
    x_min = inf;
    x_max = -inf;

   
    for i = 1:10
        Fdata = table2array(Data(table2array(Data(:,4)) == i, :));
        Fdata_all{i} = Fdata;

       
        x_min = min(x_min, min(Fdata(:,2)));
        x_max = max(x_max, max(Fdata(:,2)));
    end

   
    x_common = linspace(x_min, x_max, 100); 

   
    y_matrix = zeros(length(x_common), 10); 

    for i = 1:10
        x_i = Fdata_all{i}(:,2);
        y_i = Fdata_all{i}(:,3);

        
        y_interp = interp1(x_i, y_i, x_common, 'linear', 'extrap');
        y_matrix(:, i) = y_interp;
    end

    y_min = min(y_matrix, [], 2);  
    y_max = max(y_matrix, [], 2);  
    y_mean = mean(y_matrix, 2);    

    
    fill([x_common, fliplr(x_common)], [y_min; flipud(y_max)]', 'b', 'FaceAlpha', 0.2, 'EdgeColor', 'none');

    
    plot(x_common, y_mean, 'b', 'LineWidth', 2)

    ylabel('F^{14}C_{atm}')
    xlabel(LargeVar{j,2})
    set(gca, 'fontsize', 12)
    box on

    hold off



end

legend('Range from 10-fold cross-validation runs','Median of 10 runs')
% print('-r1200','-djpeg',['ALLargePdP.jpg'])
% close all






