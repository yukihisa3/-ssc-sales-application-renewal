/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    ＰＣ手書連携処理                     *  ./
/. *   JOB-ID      :    PPD01600                             *  ./
/. *   JOB-NAME    :    ＰＣ側手書取込量販店ＪＮＬ変換　　　 *  ./
/. ***********************************************************  ./
    PGM
    VAR       ?PGMEC    ,INTEGER
    VAR       ?PGMECX   ,STRING*11
    VAR       ?PGMEM    ,STRING*99
    VAR       ?MSG      ,STRING*99(6)
    VAR       ?MSGX     ,STRING*99
    VAR       ?PGMID    ,STRING*8,VALUE-'CSKTCNV '
    VAR       ?STEP     ,STRING*8
/.##ﾌｧｲﾙ名称編集用##./
    VAR       ?WKSTN    ,NAME
    VAR       ?WS       ,STRING*8,VALUE-'        '
    VAR       ?MEMO     ,STRING*5,VALUE-'     '
    VAR       ?BUMON    ,STRING*4,VALUE-'    '
    VAR       ?TANCD    ,STRING*2,VALUE-'  '       /.担当者CD./
    VAR       ?TOKCD    ,STRING*8,VALUE-'        ' /.取引先CD./
    VAR       ?SMEMO    ,STRING*4,VALUE-'    '     /.開始メモ_./
    VAR       ?EMEMO    ,STRING*4,VALUE-'    '     /.終了メモ_./

    VAR       ?BUNCD    ,STRING*4,VALUE-'    '     /.分類コード./
    VAR       ?DENKU    ,STRING*2,VALUE-'  '       /.伝票区分./

    VAR       ?PGNM     ,STRING*40                 /.ﾒｯｾｰｼﾞ1    ./
    VAR       ?KEKA1    ,STRING*40                 /.      2    ./
    VAR       ?KEKA2    ,STRING*40                 /.      3    ./
    VAR       ?KEKA3    ,STRING*40                 /.      4    ./
    VAR       ?KEKA4    ,STRING*40                 /.      5    ./

    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##ﾌﾟﾛｸﾞﾗﾑ名称ｾｯﾄ##./
    ?PGNM :=  'ＰＣ作成　手書伝票データ再チェック処理'

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKFLIB/TOKDTLIB/TOKSOLIB/TOKMDLIB
/.##ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##部門ｺｰﾄﾞ取得##./
SKY1602B:

    ?STEP :=   'SKY1602B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL      PGM-SKY1602B.TOKELIBO,PARA-(?WS,?BUMON)
    IF        @PGMEC    ^=   0
          THEN
              ?KEKA4  :=  '【部門ＣＤ取得】'
              GOTO ABEND
    END

/.##担当者CD指定##./
/.##更新担当者を入力させる（追加）データ担当者は全て更新担当者に##./
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
                 ?KEKA4  :=  '【担当者ＣＤ入力】'
                  GOTO  ABEND
              END
    ELSE
              ?MSGX :=  '##担当者 = ' && ?TANCD && ' ##'
              SNDMSG    ?MSGX,TO-XCTL
              ?MSGX :=  '##部門CD = ' && ?BUMON && ' ##'
              SNDMSG    ?MSGX,TO-XCTL
    END

/.##処理確認##./
SSY0607I:

    ?STEP :=   'SSY0607I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIBO
    OVRF FILE-PCRYOJL4,TOFILE-PCRYOJL4.TOKKLIB
    OVRF FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    CALL      PGM-SPD0140I.TOKELIBO,PARA-(?TOKCD,?SMEMO,?EMEMO,
                                          ?BUNCD,?DENKU)
    IF        @PGMEC     =   4010 THEN
              SNDMSG MSG-'## 取消終了 ##',TO-XCTL
              RETURN     END
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4  :=  '【変換範囲指定】'
              GOTO ABEND END

/.  ?MSGX :=  'PARA-BUNCD = ' && ?BUNCD ./
/.  SNDMSG    ?MSGX,TO-XCTL             ./
/.  ?MSGX :=  'PARA-DENKU = ' && ?DENKU ./
/.  SNDMSG    ?MSGX,TO-XCTL             ./

/.##備考ワーク初期化##./
PCLRFILE:

    ?STEP :=   'PCLRFILE'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##量販店備考ﾜｰｸ初期化##./
    CLRFILE FILE-PCUBIKF.TOKWLIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4  :=  '【備考Ｆ初期化】'
              GOTO ABEND END

/.##量販データＦコンバート##./
SPD0160B:

    ?STEP :=   'SPD0160B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-PCRYOJL4,TOFILE-PCRYOJL4.TOKKLIB
    OVRF      FILE-PCURIWK,TOFILE-PCURIWK.TOKWLIB
    OVRF      FILE-PCUBIKL1,TOFILE-PCUBIKL1.TOKWLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-TENMS1,TOFILE-TENMS1.TOKFLIB
    CALL      PGM-SPD0160B.TOKELIBO,PARA-(?TOKCD,?SMEMO,?EMEMO,
                                          ?BUNCD,?DENKU)
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4  :=  '【量販店ＪＮＬ変換】'
              GOTO ABEND END

/.##SORT1##./
SORT1:

    ?STEP :=   'SORT1   '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

/.取引先、納品日、出荷場所、伝票番号、店舗コード、行番号./
     SORT     INFILE-PCURIWK.TOKWLIB,INRL-1020,INBF-1,
              OUTFILE-PCURIWK.TOKWLIB,OUTBF-1,
              KEY-1!8!CA,KEY1-54!5!PA,KEY2-38!2!CA,KEY3-9!9!CA,
/.************KEY4-33!5!CA,KEY5-18!2!CA,***./
              KEY4-33!5!CA,KEY5-199!13!CA,KEY6-18!2!CA,
              SEQ-@YES,RCDL-@DSP
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4  :=  '【量販店データＳＯＲＴ】'
              GOTO ABEND END


/.##伝票_ＣＮＴ##./
SPD0170B:

    ?STEP :=   'SPD0170B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-PCURIWK,TOFILE-PCURIWK.TOKWLIB
    OVRF      FILE-HDENJNL,TOFILE-SHTDENF.TOKFLIB
    OVRF      FILE-PCUBIKL1,TOFILE-PCUBIKL1.TOKWLIB
    OVRF      FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF      FILE-ZAMZAIL1,TOFILE-ZAMZAIL1.TOKFLIB
    OVRF      FILE-PCDMEISF,TOFILE-PCDMEISF.TOKWLIB
    OVRF      FILE-TANMS1,TOFILE-TANMS1.TOKKLIB
    OVRF      FILE-PCKANNL1,TOFILE-PCKANNL1.TOKKLIB
    CALL      PGM-SPD0170B.TOKELIBO,PARA-(?WS,?BUMON,?TANCD
                                         ,?SMEMO,?EMEMO)
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4  :=  '【伝票番号採番】'
              GOTO ABEND END

/.##量販店ＪＮＬ削除##./
SPD0150B:

    ?STEP :=   'SPD0150B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-PCRYOJL4,TOFILE-PCRYOJL4.TOKKLIB
    CALL      PGM-SPD0150B.TOKELIBO,PARA-(?TOKCD,?SMEMO,?EMEMO)
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4  :=  '【変換ＤＴ削除】'
              GOTO ABEND END

RTN:

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
              /.１２３４５６７８９０１２３４５６７８９０./
    ?KEKA1 :=  'ＰＣ側手書データ取込（再チェック）が正'
    ?KEKA2 :=  '常に終了しました。取込確認リストにてエ'
    ?KEKA3 :=  'ラーがなくなった事を確認して下さい。'
    ?KEKA4 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC

ABEND:

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
              /.１２３４５６７８９０１２３４５６７８９０./
    ?KEKA1 :=  'ＰＣ側手書データ取込（再チェック）が異'
    ?KEKA2 :=  '常終了しました。ログを採取してＮＡＶへ'
    ?KEKA3 :=  '連絡して下さい。'
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