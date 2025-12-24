/. ***********************************************************  ./
/. *   サカタのタネ　基幹システム（本社システム）            *  ./
/. *   SYSTEM-NAME :    グッデイＥＤＩ　　　　　　　　　　   *  ./
/. *   JOB-NAME    :    受領ＣＳＶデータ出力　　　　　　　　 *  ./
/. *   JOB-ID      :    PSY57700                             *  ./
/. ***********************************************************  ./
    PGM
/.###ﾜｰｸｴﾘｱ定義####./
    VAR ?WS       ,STRING*8,VALUE-'        '    /.ﾜｰｸｽﾃｰｼｮﾝ文字./
    VAR ?WKSTN    ,NAME!MOD                     /.ﾜｰｸｽﾃｰｼｮﾝ名前./
    VAR ?PGMEC    ,INTEGER                      /.ﾌﾟﾛｸﾞﾗﾑｴﾗｰｺｰﾄﾞ./
    VAR ?PGMES    ,STRING*5,VALUE-'     '       /.ﾌﾟﾛｸﾞﾗﾑｴﾗｰｺｰﾄﾞ./
    VAR ?PGMECX   ,STRING*11                    /.ｼｽﾃﾑｴﾗｰｺｰﾄﾞ./
    VAR ?PGMEM    ,STRING*99                    /.ｼｽﾃﾑｴﾗｰﾒｯｾｰｼﾞ./
    VAR ?MSG      ,STRING*99(6)                 /.ﾒｯｾｰｼﾞ格納ﾃｰﾌﾞﾙ./
    VAR ?MSGX     ,STRING*99                    /.SNDMSG表示用./
    VAR ?PGMID    ,STRING*8,VALUE-'PSY57700'    /.ﾌﾟﾛｸﾞﾗﾑID./
    VAR ?STEP     ,STRING*8                     /.STEP-ID./
    VAR ?KENDTS   ,STRING*8,VALUE-'00000000'    /.検品日（開）./
    VAR ?KENDTE   ,STRING*8,VALUE-'00000000'    /.検品日（終）./
    VAR ?TENCDS   ,STRING*3,VALUE-'000'         /.店舗　（開）./
    VAR ?TENCDE   ,STRING*3,VALUE-'000'         /.店舗　（終）./
    VAR ?DENKU    ,STRING*2,VALUE-'00'          /.伝票区分　　./
    VAR ?PGNM     ,STRING*40                    /.ﾒｯｾｰｼﾞ1    ./
    VAR ?KEKA1    ,STRING*40                    /.      2    ./
    VAR ?KEKA2    ,STRING*40                    /.      3    ./
    VAR ?KEKA3    ,STRING*40                    /.      4    ./
    VAR ?KEKA4    ,STRING*40                    /.      5    ./

    VAR ?SYORINM1 ,STRING*40
    VAR ?SYORINM2 ,STRING*50
    VAR ?SYORINM3 ,STRING*50
    VAR ?SYORIKN1 ,STRING*7,VALUE-'0000000'     /.処理件数１ ./
    VAR ?SYORIKN2 ,STRING*7,VALUE-'0000000'     /.処理件数２ ./
    VAR ?SYORIKN3 ,STRING*7,VALUE-'0000000'     /.処理件数３ ./
    VAR ?SYORIKN4 ,STRING*7,VALUE-'0000000'     /.処理件数４ ./
    VAR ?SYORIKN5 ,STRING*7,VALUE-'0000000'     /.処理件数５ ./

/.##実行PG名称ｾｯﾄ##./
    ?PGNM := 'グッデイ受領ＣＳＶデータ作成'
/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL TOKELIB/TOKFLIB/TOKKLIB/TOKWLIB
/.##ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ##./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL
/.## ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##グッデイ受領ＣＳＶデータ作成指示##./
SSY5770I:

    ?STEP :=   'SSY5770I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIBO
  /.OVRF      FILE-GDJYURL2,TOFILE-GDJYURL2.TOKKLIB ./
    CALL      PGM-SSY5770I.TOKELIBO
             ,PARA-(?KENDTS,?KENDTE,?TENCDS,?TENCDE,?DENKU)
    ?PGMEC    :=   @PGMEC
    ?PGMES    :=   @PGMES
    IF        ?PGMEC    ^=   0    THEN
         IF   ?PGMEC     =   4010 THEN
              SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
              RETURN PGMEC-?PGMEC
         ELSE
              ?KEKA4 :=  '受領ＣＳＶデータ作成指示'
              GOTO ABEND
         END
    END
/.##ﾊﾟﾗﾒﾀ内容を表示する##./
    ?MSGX :=  '*検品日(開) = '  && ?KENDTS
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '*検品日(終) = '  && ?KENDTE
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '*店舗　(開) = '  && ?TENCDS
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '*店舗　(終) = '  && ?TENCDE
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '*伝票区分　 = '  && ?DENKU
    SNDMSG    ?MSGX,TO-XCTL

/.##受領ＣＳＶデータ出力##./
SSY5771V:

    ?STEP :=   'SSY5771V'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

  /.OVRF      FILE-GDJYURL2,TOFILE-GDJYURL2.TOKKLIB ./
  /.OVRF      FILE-GDJYRCSV,TOFILE-GDJYRCSV.TOKWLIB ./
    CALL      PGM-SSY5771V.TOKELIBO,
                  PARA-(?KENDTS,?KENDTE,?TENCDS,?TENCDE,?DENKU)
    ?PGMEC    :=   @PGMEC
    ?PGMES    :=   @PGMES
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '受領ＣＳＶデータ出力'
              GOTO ABEND END

/.##受領ＣＳＶデータ転送##./
PCFF:

    ?STEP :=   'PCFF    '
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    FIMPORT   FILE-GDJYRCSV.TOKWLIB,
              TYPE-@FILE,
              PARA-GDJYRCSV,
              UNIT-1
    ?PGMEC    :=   @PGMEC
    ?PGMES    :=   @PGMES
    IF        ?PGMEC    ^=   0    THEN
              ?KEKA4 :=  '受領ＣＳＶデータ転送'
              GOTO ABEND END

RTN:

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    ?KEKA1 :=  'グッデイ受領ＣＳＶデータ出力が'
    ?KEKA2 :=  '正常終了しました。'
    ?KEKA3 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('1',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

 /. DEFLIBL TOKELIBO/TOKELIB/TOKFLIB
    OVRPRTF FILE-PRTF,TOFILE-PRTF.TOKELIB,MEDLIB-TOKELIBO
    ?SYORINM1 := ?PGNM
    ?SYORINM2 := '正常終了しました。　　　　　　　　　　　　　　'
    ?SYORINM3 := '確定件数を確認して下さい。　　　'
    CALL SSN0050L.TOKELIBO,
          PARA-(?SYORINM1,?SYORINM2,?SYORINM3,?SYORIKN1,?SYORIKN2,
                ?SYORIKN3,?SYORIKN4,?SYORIKN5)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL              ./

    RETURN    PGMEC-?PGMEC


ABEND:

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    ?KEKA1 :=  '処理が異常終了しました。'
    ?KEKA2 :=  'ログリストを採取後、ＮＡＶへ連絡'
    ?KEKA3 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('2',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)

  /.DEFLIBL TOKELIBO/TOKELIB/TOKFLIB
    OVRPRTF FILE-PRTF,TOFILE-PRTF.TOKELIB,MEDLIB-TOKELIBO
    ?SYORINM1 := ?PGNM
    ?SYORINM2 := '異常終了しました。'
    ?SYORINM3 := 'ログを採取しＮＡＶへ送信して下さい。'
    CALL SSN0050L.TOKELIBO,
          PARA-(?SYORINM1,?SYORINM2,?SYORINM3,?SYORIKN1,?SYORIKN2,
                ?SYORIKN3,?SYORIKN4,?SYORIKN5)
    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL      ./

  /.?PGMEC    :=    @PGMEC ./
  /.?PGMEM    :=    @PGMEM ./
    ?PGMECX   :=    %STRING(?PGMEC)
    ?MSG(1)   :=    '### ' && ?PGMID && ' ABEND' && ' ###'
    ?MSG(2)   :=    '### ' && ' PGMEC = ' &&
                     %SBSTR(?PGMECX,8,4) && ' ###'
    ?MSG(3)   :=    '###' && ' LINE = '  && %LAST(LINE)      && ' ###'
    FOR ?I    :=     1 TO 3
        DO     ?MSGX :=   ?MSG(?I)
               SNDMSG    ?MSGX,TO-XCTL
    END

    RETURN    PGMEC-?PGMEC
