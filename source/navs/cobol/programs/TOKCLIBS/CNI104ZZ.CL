/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    販売管理                             *  ./
/. *   JOB-ID      :    CNI104ZZ                             *  ./
/. *   JOB-NAME    :    自動月次更新処理                     *  ./
/. ***********************************************************  ./
    PGM
/.###ﾜｰｸｴﾘｱ定義####./
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'CNI104ZZ'
    VAR       ?STEP     ,STRING*8
    VAR       ?JBNM     ,STRING*24!MIXED            /.業務漢字名 ./
    VAR       ?JBID     ,STRING*10                  /.業務ＩＤ   ./
    VAR       ?PGID     ,STRING*10                  /.ＰＧＩＤ　 ./
/.-----------------------------------------------------------./
    VAR       ?CLID     ,STRING*8                   /.ＣＬＩＤ   ./
    VAR       ?MSG1     ,STRING*80                  /.開始終了MSG./
    VAR       ?OPR1     ,STRING*50                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?OPR2     ,STRING*50                  /.      2    ./
    VAR       ?OPR3     ,STRING*50                  /.      3    ./
    VAR       ?OPR4     ,STRING*50                  /.      4    ./
    VAR       ?OPR5     ,STRING*50                  /.      5    ./
/.-----------------------------------------------------------./
    VAR       ?PGNM     ,STRING*40                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?KEKA1    ,STRING*40                  /.      2    ./
    VAR       ?KEKA2    ,STRING*40                  /.      3    ./
    VAR       ?KEKA3    ,STRING*40                  /.      4    ./
    VAR       ?KEKA4    ,STRING*40                  /.      5    ./
    VAR       ?OK       ,STRING*2,VALUE-'OK'        /.OK./
    VAR       ?NG       ,STRING*2,VALUE-'NG'        /.NG./

/.PG名称ｾｯﾄ./

    ?PGNM  :=  '出荷検品－月次更新処理'


/.#################################################################./
/.##資源の事前獲得（下記ファイル使用時、メッセージ表示）         ##./
/.#################################################################./
FILECHK:
    DEFLIBL TOKFLIB/TOKELIB

/.売上ファイル消込み./
SGE0010B:

    ?STEP :=   'SGE0010B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    SNDMSG MSG-'#＃売上ファイル　　消込中＃',TO-XCTL

    OVRF      FILE-DENJNL4,TOFILE-SHTDENL4.TOKFLIB
    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL      PGM-SGE0010B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4  :=  '【売上Ｆ　削除】'
              GOTO ABEND END

/.売上ファイル圧縮処理./
CNDPF:

    ?STEP :=   'CNDPF   '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    SNDMSG MSG-'#＃売上ファイル　　圧縮中＃',TO-XCTL

    DACTLF FILE-SHTDENL1.TOKFLIB
    DACTLF FILE-SHTDENL2.TOKFLIB
    DACTLF FILE-SHTDENL3.TOKFLIB
    DACTLF FILE-SHTDENL4.TOKFLIB
    DACTLF FILE-SHTDENL5.TOKFLIB
    DACTLF FILE-SHTDENL6.TOKFLIB
    DACTLF FILE-SHTDENL9.TOKFLIB
    DACTLF FILE-SHTDENLA.TOKFLIB
    DACTLF FILE-SHTDENLB.TOKFLIB
    DACTLF FILE-SHTDENLC.TOKFLIB
    DACTLF FILE-SHTDENLD.TOKFLIB
    CNDPF FILE-SHTDENF.TOKFLIB
    ACTLF FILE-SHTDENL1.TOKFLIB
    ACTLF FILE-SHTDENL2.TOKFLIB
    ACTLF FILE-SHTDENL3.TOKFLIB
    ACTLF FILE-SHTDENL4.TOKFLIB
    ACTLF FILE-SHTDENL5.TOKFLIB
    ACTLF FILE-SHTDENL6.TOKFLIB
    ACTLF FILE-SHTDENL9.TOKFLIB
    ACTLF FILE-SHTDENLA.TOKFLIB
    ACTLF FILE-SHTDENLB.TOKFLIB
    ACTLF FILE-SHTDENLC.TOKFLIB
    ACTLF FILE-SHTDENLD.TOKFLIB

/.条件ファイル最終月次更新日セット./
SGE0020B:

    ?STEP :=   'SGE0020B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    SNDMSG MSG-'#＃条件ファイル繰り越し中＃',TO-XCTL

    OVRF FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL  SGE0020B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4  :=  '【条件Ｆ　繰越中】'
              GOTO ABEND END

RTN:

/.##正常セット##./
    ?KEKA1 :=  '処理が正常終了しました。'
    ?KEKA2 :=  '更新結果等を確認して下さい。'
    ?KEKA3 :=  '売上データ削除'
    ?KEKA4 :=  '（正常終了）'
    CALL SMG0020L.TOKELIB
                    ,PARA-(?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL
    RETURN    PGMEC-@PGMEC

ABEND:

/.##異常セット##./
    ?KEKA1 :=  '月次更新処理が異常終了しました。'
    ?KEKA2 :=  'この画面より結果リストを出力して下さい。'
    ?KEKA3 :=  '（ログリストを採取しＮＡＶへ連絡）'
    CALL SMG0020L.TOKELIB
                    ,PARA-(?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
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

    RETURN    PGMEC-@PGMEC

