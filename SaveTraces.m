function ret = SaveTraces()
    ret=0;
    for i=2:10
        if i==41
            continue;
        end;
        filename_path='Z:\Data\Tempo\Italy\1-10\';
        filename_base=['m9c0r', int2str(i)];
        filename=[filename_path, filename_base, '.htb'];
        myfid=HTBOPEN(filename);
        ndbs = htbCount(myfid);
        for ii=1:ndbs
            temp_hd=htbGetHd(myfid, ii);
            if strcmp(temp_hd.title, 'Eye Traces')
                data=htbGetDa(temp_hd);
                HTBCLOSE(myfid);
                csvwrite(['Z:\Users\JiaJing\z_TempOutputs\', filename_base, '_Raw_Traces.csv'], data);
                break;
            end;
        end;
        i
    end;
    

