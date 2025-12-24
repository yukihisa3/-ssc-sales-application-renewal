/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    販売管理                             *  ./
/. *   JOB-ID      :    CGE00100                             *  ./
/. *   JOB-NAME    :    伝票ファイル削除（月次更新処理）     *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'CGE00100'
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

/.PG名称ｾｯﾄ./

    ?PGNM  :=  '出荷検品－月次更新処理'

/.  月次更新　起動確認メッセージ                                 ./
STEP00:
    ?OPR1  :=  '　＃＃＃＃＃　月　次　更　新　処　理　＃＃＃＃＃　'
    ?OPR2  :=  '　月次更新処理を行います。'
    ?OPR3  :=  '　月次更新前処理は完了済ですか？'
    ?OPR4  :=  '　全端末は未使用状態ですか？　　確認して下さい。'
    ?OPR5  :=  '　＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃　'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.#################################################################./
/.##資源の事前獲得（下記ファイル使用時、メッセージ表示）         ##./
/.#################################################################./
FILECHK:
    DEFLIBL TOKFLIB/TOKELIB
    ASSIGN FILE-SHTDENF.TOKFLIB!@XCL/ZAMZAIF.TOKFLIB!@XCL/
                ZAMJISF.TOKFLIB!@XCL
    IF     @PGMEC  ^=    0    THEN    GOTO   ERR  END

/.資源の獲得./
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
              ?KEKA4  :=  '【発注Ｆ　完了区分セット】'
              GOTO ABEND END

/.作業実績ファイル消込み./
SZA0150B:

    ?STEP :=   'SZA0150B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    SNDMSG MSG-'#＃作業実績ファイル消込中＃',TO-XCTL

    CALL  SZA0150B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4  :=  '【作業実績Ｆ　削除処理】'
              GOTO ABEND END
/.入出庫ファイル消込み./
SZA0160B:

    ?STEP :=   'SZA0160B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    SNDMSG MSG-'#＃入出庫ファイル　消込中＃',TO-XCTL

    CALL  SZA0160B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4  :=  '【入出庫Ｆ　削除処理】'
              GOTO ABEND END

/.発注ファイル消込み./
SZA0170B:

    ?STEP :=   'SZA0170B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    SNDMSG MSG-'#＃発注ファイル　　消込中＃',TO-XCTL

    CALL  SZA0170B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4  :=  '【入出庫Ｆ　削除処理】'
              GOTO ABEND END
/.在庫マスタ月次繰り越し./
SZA0180B:

    ?STEP :=   'SZA0180B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    SNDMSG MSG-'＃在庫マスタ　繰り越し中＃',TO-XCTL

    OVRF  JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    OVRF  ZZAIMS1,TOFILE-ZZAIMS1.TOKFLIB

    CALL  SZA0180B.TOKELIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4  :=  '【在庫Ｍ　月次繰越】'
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

    ?KEKA1 :=  '処理が正常終了しました。'
    ?KEKA2 :=  '更新結果等を確認して下さい。'
    ?KEKA3 :=  ''
    ?KEKA4 :=  '（正常終了）'
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL
    RETURN    PGMEC-@PGMEC

ABEND:
    ?KEKA1 :=  '月次更新処理が異常終了しました。'
    ?KEKA2 :=  'この画面より結果リストを出力して下さい。'
    ?KEKA3 :=  '（ログリストを採取しＮＡＶへ連絡）'
    CALL SMG0030I.TOKELIB
                    ,PARA-('2',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
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

/.#################################################################./
/.##資源使用時、他端末開放確認メッセージ出力）                   ##./
/.#################################################################./
ERR:  /.資源の解放./

    ?OPR1  :=  '＃＃＃＃＃＃＃　資源使用中　＃＃＃＃＃＃＃＃＃＃＃'
    ?OPR2  :=  '　　　　　　　　　　　　　　　　　　　　　　　　　'
    ?OPR3  :=  '他端末にて資源を使用中です。　　　　　　　　　　　'
    ?OPR4  :=  '全端末の使用状況を確認して下さい。　　　　　　　　'
    ?OPR5  :=  '【ＥＮＴＥＲでリスタート，ＰＦ９でＰＧ終了】　　　'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

    GOTO      FILECHK
