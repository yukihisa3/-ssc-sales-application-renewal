/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    出荷管理                             *  ./
/. *   JOB-ID      :    PPD01000                             *  ./
/. *   JOB-NAME    :    ＰＣ側手書データ取込（テスト）       *  ./
/. ***********************************************************  ./
    PGM

    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'PPD01000'
    VAR       ?STEP     ,STRING*8
    VAR       ?BUMON    ,STRING*4,VALUE-'    '     /.部門名./
    VAR       ?TANCD    ,STRING*2,VALUE-'  '       /.担当者CD./
    VAR       ?WS       ,STRING*8,VALUE-'        ' /.ﾜｰｸｽﾃｰｼｮﾝ文字./
    VAR       ?WKSTN    ,NAME!MOD                  /.ﾜｰｸｽﾃｰｼｮﾝ名前./
    VAR       ?LSTTIME  ,STRING*6,VALUE-'      '   /.時刻./
    VAR       ?LSTDATE  ,STRING*8,VALUE-'        ' /.日付./
    VAR       ?MEMO1    ,STRING*4,VALUE-'    '     /.開始メモ_./
    VAR       ?MEMO2    ,STRING*4,VALUE-'    '     /.終了メモ_./
    VAR       ?OPR1     ,STRING*50                 /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?OPR2     ,STRING*50                 /.      2    ./
    VAR       ?OPR3     ,STRING*50                 /.      3    ./
    VAR       ?OPR4     ,STRING*50                 /.      4    ./
    VAR       ?OPR5     ,STRING*50                 /.      5    ./
    VAR       ?PGNM     ,STRING*40                 /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?KEKA1    ,STRING*40                 /.      2    ./
    VAR       ?KEKA2    ,STRING*40                 /.      3    ./
    VAR       ?KEKA3    ,STRING*40                 /.      4    ./
    VAR       ?KEKA4    ,STRING*40                 /.      5    ./
    VAR       ?REPLKEKA ,STRING*1,VALUE-' '

/.## ﾌﾟﾛｸﾞﾗﾑ開始MSG ##./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  'ＰＣ作成　手書伝票データ取込'

/.## ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKFLIB/TOKKLIB/TOKELIBO

/.##確認画面##./
SHOM0900:

    ?STEP :=   'SHOM0900'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    ?OPR1  :=  '処理名：ＰＣ側手書データ取込'
    ?OPR2  :=  '　ＰＣ側で作成された手書データを取込みます。'
    ?OPR3  :=  '　完了後、取込内容を確認して下さい。また、エ'
    ?OPR4  :=  '　ラーがある場合は、専用の量販店入力画面で修'
    ?OPR5  :=  '　正して下さい。'
    CALL      OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)
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
    ELSE
              ?MSGX :=  '##部門 = ' && ?BUMON && ' ##'
              SNDMSG    ?MSGX,TO-XCTL
              ?MSGX :=  '##担当 = ' && ?TANCD && ' ##'
              SNDMSG    ?MSGX,TO-XCTL
    END

/.##手書ファイル初期化##./
PCLRFILE:

    ?STEP :=   'PCLRFILE'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CLRFILE FILE-PCTJISSF.TOKKLIB
    IF        @PGMEC    ^=   0  THEN
              ?KEKA4 :=  '【手書データ（受信先）初期化】'
              GOTO  ABEND
    END

/.##レプリカ開始##./
REPLT010:

    ?STEP :=   'REPLT010'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

/.  SYNCREPL GRP-'TEGAKI-DENPYO-REPLICA',MODE-@ALL,WAIT-@YES
 ./ CALL REPLT020.TOKCLIBO,PARA-(?REPLKEKA)
    IF        @PGMEC    ^=   0  THEN
              ?KEKA4 :=  '【手書データ（レプリカ）】'
              GOTO  ABEND
    END
    IF        ?REPLKEKA  =   '2'  THEN
              ?KEKA4 :=  '【手書データ（レプリカ）】'
              GOTO  ABEND
    END

/.##受信ファイルコピー##./
PCNVFILE:

    ?STEP :=   'PCNVFILE'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    CNVFILE FILE-PCTJISSF.TOKKLIB,TOFILE-PCDATAWK.TOKKLIB,
            ADD-@NO,BF-20
    IF        @PGMEC    ^=   0  THEN
              ?KEKA4 :=  '【手書データ（受信先）初期化】'
              GOTO  ABEND
    END

/.##ＳＯＲＴ##./
SORT1:
/.##ＳＯＲＴ順：取引先ＣＤ、納品日、相手商品、店舗順##./
    ?STEP :=  'SORT1   '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    SORT       INFILE-PCDATAWK.TOKKLIB,INRL-200,INBF-20,
              OUTFILE-PCDATAWK.TOKKLIB,OUTBF-20,
              KEY-5!8!CA,KEY1-42!8!CA,KEY2-58!13!CA,KEY3-13!5!CA,
              KEY4-21!5!CA,
              RCDL-@DSP

    IF        @PGMEC    ^=   0    THEN
              GOTO ABEND
    END

/.##PC側手書取込データ量販店DT作成##./
SPD0100B:

    ?STEP :=   'SPD0100B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRF      FILE-PCDATAWK,TOFILE-PCDATAWK.TOKKLIB
    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    OVRF      FILE-MEIMS1,TOFILE-MEIMS1.TOKFLIB
    OVRF      FILE-TENMS1,TOFILE-TENMS1.TOKFLIB
    OVRF      FILE-PCRYOJL1,TOFILE-PCRYOJL1.TOKKLIB
    CALL      PGM-SPD0100B.TOKELIBO,PARA-(?BUMON,?TANCD,?MEMO1,
                                          ?MEMO2)
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【手書伝票データ取込処理】'
              GOTO ABEND
    ELSE
              ?MSGX :=  'MEMO1 = ' && ?MEMO1 && ' MEMO2 = ' && ?MEMO2
              SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    END

/.##PC側手書取込データ量販店DTﾁｪｯｸ##./
SPD0110B:

    ?STEP :=   'SPD0110B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-TANMS1,TOFILE-TANMS1.TOKKLIB
    OVRF      FILE-PCRYOJL1,TOFILE-PCRYOJL1.TOKKLIB
    CALL      PGM-SPD0110B.TOKELIBO,PARA-(?BUMON,?TANCD,?MEMO1,
                                          ?MEMO2)
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【取込データエラーチェック処理】'
              GOTO ABEND
    END

/.##手書取込確認リスト兼エラーリスト##./
SPD0115L:

    ?STEP :=   'SPD0115L'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-TANMS1,TOFILE-TANMS1.TOKKLIB
    OVRF      FILE-SHOTBL1,TOFILE-SHOTBL1.TOKFLIB
    OVRF      FILE-PCRYOJL1,TOFILE-PCRYOJL1.TOKKLIB
    CALL      PGM-SPD0115L.TOKELIBO,PARA-(?BUMON,?TANCD,?MEMO1,
                                          ?MEMO2)
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4 :=  '【取込確認リスト兼エラーリスト】'
              GOTO ABEND
    END

RTN:

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
              /.１２３４５６７８９０１２３４５６７８９０./
    ?KEKA1 :=  'ＰＣ側の手書データの取込みが正常に終了'
    ?KEKA2 :=  'しました。取込確認後、取込確認リストを'
    ?KEKA3 :=  '発行して取込内容を確認して下さい。'
    ?KEKA4 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

    RETURN    PGMEC-@PGMEC

ABEND:

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
              /.１２３４５６７８９０１２３４５６７８９０./
    ?KEKA1 :=  'ＰＣ側の手書データの取込みが異常終了し'
    ?KEKA2 :=  'ました。ログリストを採取してＮＡＶへ連'
    ?KEKA3 :=  '絡して下さい。'
    CALL SMG0030I.TOKELIB
                    ,PARA-('2',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMECX   :=    %STRING(?PGMEC)
    ?MSG(1)   :=    '### ' && ?PGMID && ' ABEND' && ' ###'
    ?MSG(2)   :=    '### ' && ' PGMEC = ' &&
                     %SBSTR(?PGMECX,8,4) && ' ###'
    ?MSG(3)   :=    '###' && ' LINE = '  && %LAST(LINE)      && ' ###'
    FOR ?I    :=     1 TO 3
        DO     ?MSGX :=   ?MSG(?I)
               SNDMSG    ?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    END

    RETURN    PGMEC-@PGMEC
