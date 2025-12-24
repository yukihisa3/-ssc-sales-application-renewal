/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    販売管理                             *  ./
/. *   JOB-ID      :    CGE00110                             *  ./
/. *   JOB-NAME    :    伝票ファイル削除（月次更新処理）     *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'CGE00110'
    VAR       ?STEP     ,STRING*8
    VAR       ?JBNM     ,STRING*24!MIXED            /.業務漢字名 ./
    VAR       ?JBID     ,STRING*10                  /.業務ＩＤ   ./
    VAR       ?PGNM     ,STRING*24!MIXED            /.ＰＧ漢字名 ./
    VAR       ?PGID     ,STRING*10                  /.ＰＧＩＤ　 ./
/.-----------------------------------------------------------./
    VAR       ?CLID     ,STRING*8                   /.ＣＬＩＤ   ./
    VAR       ?MSG1     ,STRING*80                  /.開始終了MSG./
    VAR       ?OPR1     ,STRING*50                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?OPR2     ,STRING*50                  /.      2    ./
    VAR       ?OPR3     ,STRING*50                  /.      3    ./
    VAR       ?OPR4     ,STRING*50                  /.      4    ./
    VAR       ?OPR5     ,STRING*50                  /.      5    ./

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.資源の獲得./
    DEFLIBL TOKFLIB/TOKELIB
    OVRF  SGYFILL1,TOFILE-SGYFILL1.TOKFLIB
    OVRF  NYSFILL1,TOFILE-NYSFILL1.TOKFLIB
    OVRF  NYKFILL1,TOFILE-NYKFILL1.TOKFLIB
    OVRF  HACHEDL1,TOFILE-HACHEDL1.TOKFLIB
    OVRF  HACMEIL1,TOFILE-HACMEIL1.TOKFLIB
    OVRF  JYOKEN1,TOFILE-JYOKEN1.TOKFLIB

/.発注Ｆ　完了区分セット./
SZA0171B:

    ?STEP :=   'SZA0171B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    SNDMSG MSG-'#＃発注Ｆ　完了区分セット＃',TO-XCTL

    CALL  SZA0171B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.発注ファイル消込み./
SZA0170B:

    ?STEP :=   'SZA0170B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    SNDMSG MSG-'#＃発注ファイル　　消込中＃',TO-XCTL

    CALL  SZA0170B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL
    RETURN    PGMEC-@PGMEC

ABEND:
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMECX   :=    %STRING(?PGMEC)
    ?MSG(1)   :=   '### ' && ?PGMID && ' ABEND' &&   '    ###'
    ?MSG(2)   :=   '###' && ' PGMEC = ' &&
                    %SBSTR(?PGMECX,8,4) &&         '      ###'
    ?MSG(3)   :=   '###' && ' STEP = '  && ?STEP
                                                   && '   ###'
    FOR ?I    :=     1 TO 3
        DO     ?MSGX :=   ?MSG(?I)
               SNDMSG    ?MSGX,TO-XCTL
    END

    ?OPR1  :=  '　＃＃＃＃＃　月　次　更　新　異　常　＃＃＃＃＃　'
    ?OPR2  :=  '　月次更新処理が異常終了しました。'
    ?OPR3  :=  '　ログを採取し，ナブ・アシストへ連絡して下さい。　'
    ?OPR4  :=  '　ＴＥＬ０３－５２９４－６８９１'
    ?OPR5  :=  '　＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃　'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)
    RETURN    PGMEC-@PGMEC
