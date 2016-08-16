function ret = SaveTraces()
    ret=0;
    for i=11:21
        if i==41
            continue;
        end;
        filename_path='Z:\Data\Tempo\Italy\11-21\';
        filename_base=['m9c0r', int2str(i)];
        filename=[filename_path, filename_base, '.htb'];
        myfid=HTBOPEN(filename);
        ndbs = htbCount(myfid);
        for ii=1:ndbs
            temp_hd=htbGetHd(myfid, ii);
            if strcmp(temp_hd.title, 'Events')
                data=htbGetDa(temp_hd);
                HTBCLOSE(myfid);
                csvwrite(['Z:\Users\JiaJing\z_TempOutputs\', filename_base, '_Raw_Events.csv'], data);
                break;
            end;
        end;
        i
    end;
    

