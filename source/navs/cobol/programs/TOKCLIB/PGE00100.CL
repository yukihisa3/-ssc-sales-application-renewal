/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    販売管理                             *  ./
/. *   JOB-ID      :    PGE00100                             *  ./
/. *   JOB-NAME    :    伝票ファイル削除（月次更新処理）     *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PGE00100'
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
/.  月次更新　起動確認メッセージ                                 ./
STEP00:
    ?OPR1  :=  '　＃＃＃＃＃　月　次　更　新　処　理　＃＃＃＃＃　'
    ?OPR2  :=  '　月次更新処理を行います。　　　　　　　　　　　　'
    ?OPR3  :=  '　月次更新前処理は完了済ですか？　　　　　　　　　'
    ?OPR4  :=  '　全端末は未使用状態ですか？　　確認して下さい。　'
    ?OPR5  :=  '　＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃　'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.資源の獲得./
    DEFLIBL TOKFLIB/TOKELIB
    ASSIGN FILE-SHTDENF.TOKFLIB!@XCL/ZZAIMS.TOKFLIB!@XCL
    OVRF  ZSGYODT1,TOFILE-ZSGYODT1.TOKFLIB
    OVRF  ZNYUSDT1,TOFILE-ZNYUSDT1.TOKFLIB
    OVRF  ZNYUKDT2,TOFILE-ZNYUKDT2.TOKFLIB
    OVRF  ZHACHDT2,TOFILE-ZHACHDT2.TOKFLIB
    OVRF  JYOKEN1,TOFILE-JYOKEN1.TOKFLIB

/.作業実績ファイル消込み./
ZMO0040:

    ?STEP :=   'ZMO0040B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    SNDMSG MSG-'#＃作業実績ファイル消込中＃',TO-XCTL

    CALL  ZMO0040B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END
/.入出庫ファイル消込み./
ZMO0050:

    ?STEP :=   'ZMO0050B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    SNDMSG MSG-'#＃入出庫ファイル　消込中＃',TO-XCTL

    CALL  ZMO0050B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END
/.発注ファイル消込み./
ZMO0060:

    ?STEP :=   'ZMO0060B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    SNDMSG MSG-'#＃発注ファイル　　消込中＃',TO-XCTL

    CALL  ZMO0060B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END
/.在庫マスタ月次繰り越し./
ZMO0030:

    ?STEP :=   'ZMO0030B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    SNDMSG MSG-'＃在庫マスタ　繰り越し中＃',TO-XCTL

    OVRF  JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    OVRF  ZZAIMS1,TOFILE-ZZAIMS1.TOKFLIB

    CALL  ZMO0030B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

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
    ?OPR2  :=  '　月次更新処理が異常終了しました。　　　　　　　　'
    ?OPR3  :=  '　ログを採取し，ナブ・アシストへ連絡して下さい。　'
    ?OPR4  :=  '　ＴＥＬ０３－５２９４－６８９１　　　　　　　　　'
    ?OPR5  :=  '　＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃　'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)
    RETURN    PGMEC-@PGMEC
