# PMT996

**種別**: JCL  
**ライブラリ**: TOKCLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKCLIBS/PMT996.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    マスタメンテ                         *  ./
/. *   JOB-ID      :    PMT996                               *  ./
/. *   JOB-NAME    :    _番一括更新                       *  ./
/. *               :    2011/11/10                           *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PMT996  '
    VAR       ?STEP     ,STRING*8
    VAR       ?PGNM     ,STRING*40                    /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?KEKA1    ,STRING*40                    /.      2    ./
    VAR       ?KEKA2    ,STRING*40                    /.      3    ./
    VAR       ?KEKA3    ,STRING*40                    /.      4    ./
    VAR       ?KEKA4    ,STRING*40                    /.      5    ./
    VAR       ?OPR1   ,STRING*50                  /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?OPR2   ,STRING*50                  /.      2    ./
    VAR       ?OPR3   ,STRING*50                  /.      3    ./
    VAR       ?OPR4   ,STRING*50                  /.      4    ./
    VAR       ?OPR5   ,STRING*50                  /.      5    ./
    VAR       ?BUMON    ,STRING*4,VALUE-'    '     /.部門名./
    VAR       ?TANCD    ,STRING*2,VALUE-'  '       /.担当者CD./
    VAR       ?WS       ,STRING*8,VALUE-'        ' /.ﾜｰｸｽﾃｰｼｮﾝ文字./
    VAR       ?NYURYOKU ,STRING*8,VALUE-'99999999' /.入力日./
    VAR       ?STIME    ,STRING*4,VALUE-'    '     /.時刻./
    VAR       ?WKSTN    ,NAME!MOD                  /.ﾜｰｸｽﾃｰｼｮﾝ名前./
    VAR       ?P1       ,STRING*34                 /.ﾊﾟﾗﾒﾀ./
    VAR       ?SOKCD    ,STRING*2,VALUE-'00'       /.倉庫(自)  ./
    VAR       ?DSOKCD   ,STRING*2,VALUE-'00'       /.倉庫(代表)./

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL    TOKELIB/TOKFLIB/TOKELIBO/TOKKLIB/TOKWLIB

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  '_番一括更新'

    ?OPR1  :=  '　処理名称：_番一括更新　　　　　　　'
    ?OPR2  :=  '　＜注意＞データ退避を行ないます。他端末未使用必須'
    ?OPR3  :=  '　_番一括変更入力にて作成したデータの　　'
    ?OPR4  :=  '　商品変換テーブルへの登録を行ないます。　　　　　'
    ?OPR5  :=  '　更新完了後、正しく登録されているか確認！！'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)
/.## ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##倉庫ｺｰﾄﾞ取得##./
SKY1601B:

    ?STEP :=   'SKY1601B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL      PGM-SKY1601B.TOKELIB,PARA-(?WS,?SOKCD,?DSOKCD)
    IF        @PGMEC    ^=   0   THEN
              GOTO ABEND
    END

/.##部門ｺｰﾄﾞ取得##./
SKY1602B:

    ?STEP :=   'SKY1602B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL      PGM-SKY1602B.TOKELIBO,PARA-(?WS,?BUMON)
    IF        @PGMEC    ^=   0
          THEN
              GOTO ABEND
    END

/.##担当者CD指定##./
SKY6101I:

    ?STEP :=   'SKY6101I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIBO
    OVRF      FILE-TANMS1,TOFILE-TANMS1.TOKKLIB
    CALL      PGM-SKY6101I.TOKELIBO,PARA-(?BUMON,?TANCD)
    IF        @PGMEC    ^=   0  THEN
              IF  @PGMEC = 4010  THEN
                  SNDMSG MSG-'##取消終了##',TO-XCTL
                  RETURN
              ELSE
                  GOTO  ABEND
              END
    END


/.##データバックアップ##./
DATABAK:
    ?STEP :=   'DATABAK'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

/.  SAVFILE FILE-HSHOTBL.TOKFLIB,TODEV-MO,MODE-@USED
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【商品変換データバックアップ】'
              GOTO ABEND END
 ./
/.##一括登録##./
ZMT996B:
    ?STEP :=   'ZMT996B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-TANAMTB3,TOFILE-TANAMTB3.TOKWLIB
    OVRF      FILE-SHOTBL1,TOFILE-SHOTBL1.TOKFLIB
    CALL      PGM-ZMT996B.TOKELIBO,
              PARA-(?TANCD,?BUMON,?NYURYOKU,?STIME)
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【商品変換登録更新】'
              GOTO ABEND END

/.##ファイル初期化##./
PCLRFILE:
    ?STEP :=   'PCLRFILE'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CLRFILE FILE-TANAMTBL.TOKWLIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【商品変換登録ファイル初期化】'
              GOTO ABEND END

    ?MSGX := '担当者 = ' && ?TANCD
    SNDMSG ?MSGX,TO-XCTL
    ?MSGX := '部門　 = ' && ?BUMON
    SNDMSG ?MSGX,TO-XCTL
    ?MSGX := '入力日 = ' && ?NYURYOKU
    SNDMSG ?MSGX,TO-XCTL
    ?MSGX := '時刻   = ' && ?STIME
    SNDMSG ?MSGX,TO-XCTL
    ?P1 := '1' && ?BUMON && ?TANCD && ?TANCD && ?NYURYOKU &&
           ?NYURYOKU && ' ' && ?STIME && ?STIME
    SNDMSG ?P1,TO-XCTL
/.##実行判定##./
    IF  ?NYURYOKU = '99999999'  THEN
         SNDMSG MSG-'##途中ｷｬﾝｾﾙ ##',TO-XCTL
         SNDMSG MSG-'##自動出力ﾅｼ##',TO-XCTL
    END

/.##商品変換テーブルリスト##./
SLST060:

    ?STEP :=   'SLST060 '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##本社ﾚｰｻﾞｰへ出力##./
    IF ?BUMON = '2920' THEN
       OVRPRTF FILE-XU04LP,TOFILE-KAHMAPRT.XUCL
    ELSE
       OVRPRTF   FILE-PRTF,TOFILE-XU04LP.XUCL,MEDLIB-TOKELIBO
    END
    OVRF     FILE-TANMS1,TOFILE-TANMS1.TOKKLIB
    OVRF     FILE-HSHOTBR2,TOFILE-HSHOTBR2.TOKKLIB
    OVRF     FILE-MEIMS1,TOFILE-MEIMS1.TOKFLIB
    OVRF     FILE-TANMS1,TOFILE-TANMS1.TOKKLIB
    OVRF     FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    CALL     PGM-SLST060.TOKELIBO,PARA-(?P1)
    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND END

RTN:

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    ?KEKA1 :=  '商品変換テーブルの一括登録が終了しまし'
    ?KEKA2 :=  'た。登録内容を確認して下さい。'
    ?KEKA3 :=  ''
    ?KEKA4 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC

ABEND:

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    ?KEKA1 :=  '商品変換テーブル一括登録が異常終了しま'
    ?KEKA2 :=  'した。ログリストを採取してＮＡＶまで連'
    ?KEKA3 :=  '絡をお願い致します。'
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

```
