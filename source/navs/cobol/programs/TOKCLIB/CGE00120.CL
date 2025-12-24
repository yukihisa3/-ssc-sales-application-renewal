/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    販売管理                             *  ./
/. *   JOB-ID      :    CGE00120                             *  ./
/. *   JOB-NAME    :    伝票ファイル削除（月次更新処理）     *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'CGE00120'
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
    SNDMSG    ?MSGX,TO-XCTL

/.出荷情報ＤＴ削除（０１７３）./
SKE09001:

    ?STEP :=   'SKE09001'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    SNDMSG MSG-'#＃出荷情報１　削除中＃',TO-XCTL

    OVRF FILE-RUISYUL1,TOFILE-RUISYUL1.TOKKLIB
    OVRF FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL  SKE0900B.TOKELIB,PARA-('00000173')
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.出荷情報ＤＴ削除（０７６０）./
SKE09002:

    ?STEP :=   'SKE09002'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    SNDMSG MSG-'#＃出荷情報２　削除中＃',TO-XCTL

    OVRF FILE-RUISYUL1,TOFILE-RUISYUL1.TOKKLIB
    OVRF FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL  SKE0900B.TOKELIB,PARA-('00000760')
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.出荷検収ＤＴ削除./
SKE0910B:

    ?STEP :=   'SKE0910B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    SNDMSG MSG-'#＃出荷検収　　削除中＃',TO-XCTL

    OVRF FILE-KENSOKL2,TOFILE-KENSOKL2.TOKKLIB
    OVRF FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL  SKE0910B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

/.受払累積Ｆ削除./
PJS01100:

    ?STEP :=   'PJS01100'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    SNDMSG MSG-'#＃受払累積Ｆ削除中＃',TO-XCTL

    CALL  PJS01100.TOKELIB

    ?OPR1  :=  '　＃＃＃＃＃　　受払累積Ｆ退避処理　　＃＃＃＃＃　'
    ?OPR2  :=  '　削除した受払累積Ｆデータのバックアップを行いま'
    ?OPR3  :=  '　す。再度，月次前データ退避処理で使用したＭＯを'
    ?OPR4  :=  '　セットして下さい。'
    ?OPR5  :=  '　＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃　'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

    SAVFILE FILE-GERUISKF.TOKFLIB,TODEV-MO,MODE-@USED

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