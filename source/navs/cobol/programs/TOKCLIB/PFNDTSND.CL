/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    販売管理                             *  ./
/. *   JOB-ID      :    PFNDTSND                             *  ./
/. *   JOB-NAME    :    更新                                 *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PFNDTSND'
    VAR       ?STEP     ,STRING*8
    VAR       ?MSG1     ,STRING*80                  /.開始終了MSG./
    VAR       ?OPR1     ,STRING*50                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?OPR2     ,STRING*50                  /.      2    ./
    VAR       ?OPR3     ,STRING*50                  /.      3    ./
    VAR       ?OPR4     ,STRING*50                  /.      4    ./
    VAR       ?OPR5     ,STRING*50                  /.      5    ./
    VAR       ?PGNM     ,STRING*40                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?KEKA1    ,STRING*40                  /.      2    ./
    VAR       ?KEKA2    ,STRING*40                  /.      3    ./
    VAR       ?KEKA3    ,STRING*40                  /.      4    ./
    VAR       ?KEKA4    ,STRING*40                  /.      5    ./
/.##片岡用出荷検品マスタ送信##./
STEP00:
    ?OPR1  :=  '　＃＃＃＃＃＃＃　マスタデータ送信　＃＃＃＃＃＃　'
    ?OPR2  :=  '　出荷検品用のマスタデータを送信します。'
    ?OPR3  :=  '　確認して下さい。'
    ?OPR4  :=  ''
    ?OPR5  :=  '　＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃　'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
     DEFLIBL   TOKFLIB/TOKELIB

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  '検品データ送信'

/.##店舗マスタ送信##./
PSND1:

    ?STEP :=   'PSND1   '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FIMPORT FILE-KNPTEN63.TOKFLIB,PARA-SYUKA,UNIT-8
 /. IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【店舗マスタ送信】'
              GOTO ABEND END
   ./
    FIMPORT FILE-KNPTEK63.TOKFLIB,PARA-SYUKA,UNIT-14
 /. IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【店舗マスタ件数送信】'
              GOTO ABEND END
   ./
/.##倉庫別検品取引先マスタ送信##./
PSND2:

    ?STEP :=   'PSND2   '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FIMPORT FILE-KNPKEN63.TOKFLIB,PARA-SYUKA,UNIT-10
/.  IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【倉庫別検品取引先マスタ送信】'
              GOTO ABEND END
  ./
    FIMPORT FILE-KNPKEK63.TOKFLIB,PARA-SYUKA,UNIT-15
/.  IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【倉庫別検品取引先マスタ件数送信】'
              GOTO ABEND END
  ./
/.##商品名称マスタ送信##./
PSND3:

    ?STEP :=   'PSND3   '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FIMPORT FILE-KNPSYOF.TOKFLIB,PARA-SYUKA,UNIT-11
 /. IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【商品名称マスタ送信】'
              GOTO ABEND END
   ./
    FIMPORT FILE-KNPSYKF.TOKFLIB,PARA-SYUKA,UNIT-16
 /. IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【商品名称マスタ件数送信】'
              GOTO ABEND END
   ./
/.##検品グループマスタ送信##./
PSND4:

    ?STEP :=   'PSND4   '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FIMPORT FILE-KNPKPG63.TOKFLIB,PARA-SYUKA,UNIT-12
/.  IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【検品グループマスタ送信】'
              GOTO ABEND END
  ./
    FIMPORT FILE-KNPKPK63.TOKFLIB,PARA-SYUKA,UNIT-17
 /. IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【検品グループマスタ送信】'
              GOTO ABEND END
   ./
/.##ロケーションマスタ送信##./
PSND5:

    ?STEP :=   'PSND5   '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FIMPORT FILE-KNPTAN63.TOKFLIB,PARA-SYUKA,UNIT-13
/.  IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【ロケーションマスタ送信】'
              GOTO ABEND END
  ./
    FIMPORT FILE-KNPTAK63.TOKFLIB,PARA-SYUKA,UNIT-18
 /. IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【ロケーションマスタ件数送信】'
              GOTO ABEND END
   ./
RTN:

    ?KEKA1 :=  '出荷検品マスタの送信が完了しました。'
    ?KEKA2 :=  '更新結果を確認して下さい。'
    ?KEKA3 :=  ''
    ?KEKA4 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL
    RETURN    PGMEC-@PGMEC

ABEND:

    ?KEKA1 :=  '出荷検品マスタの送信に失敗しました。'
    ?KEKA2 :=  'ログリスト等を採取し，ＮＡＶへ連絡して'
    ?KEKA3 :=  '下さい。'
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

    RETURN    PGMEC-@PGMEC
