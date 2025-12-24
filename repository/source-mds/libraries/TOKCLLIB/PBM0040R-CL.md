# PBM0040R

**種別**: JCL  
**ライブラリ**: TOKCLLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKCLLIB/PBM0040R.CL`

## ソースコード

```jcl
/. ***********************************************************  ./
/. *     サカタのタネ　特販システム（本社システム）          *  ./
/. *   SYSTEM-NAME :    出荷管理                    　　     *  ./
/. *   JOB-ID      :    PBM0040R                             *  ./
/. *   JOB-NAME    :    流通ＢＭＳ伝票発行（再発行用）　　　 *  ./
/. *     2013/01/30:      バロー追加                         *  ./
/. *     2013/03/05:      コメリ追加                         *  ./
/. *     2013/03/22:      バロー植物追加                     *  ./
/. *     2013/04/30:      ソート領域拡張                     *  ./
/. *     2018/03/13:      セキチュー追加   　　              *  ./
/. *     2018/06/26:      ムサシ追加　　　 　　              *  ./
/. *     2018/09/10:      山新追加　　　　 　　              *  ./
/. *     2019/04/19:      ジュンテンドー追加　              *  ./
/. *     2019/07/08:      カインズ追加　　　　               *  ./
/. *     2021/10/06:      ビバホーム追加　　　               *  ./
/. *     2022/01/06:      ジョイフルエーケー追加             *  ./
/. *     2022/10/20:      バロー・コメリ　実行PG変更         *  ./
/. *     2023/01/20:      アレンザＨＤ対応（２社）　　       *  ./
/. *     2023/11/27:      ハンズマン　　　　　　　　　       *  ./
/. ***********************************************************  ./
    PGM  (P1-?KBN)
/.###ﾊﾟﾗﾒﾀ定義####./
    PARA ?KBN     ,STRING*1,IN,VALUE-' ' /.1=自,2=選択,3.倉 ./
/.###ﾜｰｸｴﾘｱ定義####./
    VAR ?FILNM    ,STRING*8,VALUE-'        '
    VAR ?FILNMH   ,STRING*6,VALUE-'      '
    VAR ?FILNMD   ,STRING*6,VALUE-'BMSDEN'
    VAR ?FILNMO   ,STRING*5,VALUE-'BMSDT'
    VAR ?LIBNM    ,STRING*8,VALUE-'BMSFLIB '
    VAR ?FILID    ,NAME
    VAR ?LIBID    ,NAME
    VAR ?DENPYO   ,NAME!MOD
    VAR ?DENPYON  ,STRING*17
    VAR ?BMSDT    ,NAME!MOD
    VAR ?BMSDTN   ,STRING*17
    VAR ?BMSDT1   ,NAME!MOD
    VAR ?BMSDT1N  ,STRING*17
    VAR ?BMSDT2   ,NAME!MOD
    VAR ?BMSDT2N  ,STRING*17
    VAR ?BMSDT3   ,NAME!MOD
    VAR ?BMSDT3N  ,STRING*17
    VAR ?PGNAME   ,NAME
    VAR ?LIBNAME  ,NAME
    VAR ?OVRFNAME ,NAME
    VAR ?PGC      ,NAME!MOD
    VAR ?PGN      ,STRING*17
    VAR ?WS       ,STRING*8,VALUE-'        '    /.ﾜｰｸｽﾃｰｼｮﾝ文字./
    VAR ?WKSTN    ,NAME!MOD                     /.ﾜｰｸｽﾃｰｼｮﾝ名前./
    VAR ?PGMEC    ,INTEGER                      /.ｴﾗｰｽﾃｲﾀｽ./
    VAR ?PGMES    ,STRING*5,VALUE-'     '
    VAR ?PGMECX   ,STRING*11                    /.ｴﾗｰｽﾃｲﾀｽ変換./
    VAR ?PGMEM    ,STRING*99                    /.ｴﾗｰﾒｯｾｰｼﾞ./
    VAR ?MSG      ,STRING*99(6)                 /.ﾒｯｾｰｼﾞ定義./
    VAR ?MSGX     ,STRING*99                    /.ﾒｯｾｰｼﾞ定義変換./
    VAR ?PGMID    ,STRING*8,VALUE-'PBM0040R'    /.ﾌﾟﾛｸﾞﾗﾑID./
    VAR ?STEP     ,STRING*8                     /.ﾌﾟﾛｸﾞﾗﾑｽﾃｯﾌﾟ./
    VAR ?SOKO     ,STRING*2,VALUE-'  '          /.出荷場所./
    VAR ?SOKCD    ,STRING*2,VALUE-'  '          /.自倉庫ｺｰﾄﾞ./
    VAR ?DSOKCD   ,STRING*2,VALUE-'  '          /.代表倉庫ｺｰﾄﾞ./
    VAR ?JYUN     ,STRING*1,VALUE-' '           /.出力順./
    VAR ?DENPTN   ,STRING*1,VALUE-' '           /.伝票種類./
    VAR ?JDATE    ,STRING*8,VALUE-'00000000'    /.受信日付./
    VAR ?JTIME    ,STRING*4,VALUE-'0000'        /.受信時間./
    VAR ?TORICD   ,STRING*8,VALUE-'00000000'    /.取引先./
    VAR ?NOUDT1   ,STRING*8,VALUE-'00000000'    /.開始納品日./
    VAR ?NOUDT2   ,STRING*8,VALUE-'00000000'    /.終了納品日./
    VAR ?TENCD1   ,STRING*5,VALUE-'00000'       /.開始店舗　./
    VAR ?TENCD2   ,STRING*5,VALUE-'00000'       /.終了店舗　./
    VAR ?PGCD     ,STRING*8,VALUE-'        '    /.ﾌﾟﾛｸﾞﾗﾑCD./
    VAR ?LIBCD    ,STRING*8,VALUE-'        '    /.ﾗｲﾌﾞﾗﾘCD./
    VAR ?KEYA     ,STRING*1,VALUE-'1'           /.KEY1./
    VAR ?KEYB     ,STRING*1,VALUE-'2'           /.KEY2./
    VAR ?KEYC     ,STRING*1,VALUE-'3'           /.KEY3./
    VAR ?OPR1     ,STRING*50                    /.ﾒｯｾｰｼﾞ1    ./
    VAR ?OPR2     ,STRING*50                    /.ﾒｯｾｰｼﾞ2    ./
    VAR ?OPR3     ,STRING*50                    /.ﾒｯｾｰｼﾞ3    ./
    VAR ?OPR4     ,STRING*50                    /.ﾒｯｾｰｼﾞ4    ./
    VAR ?OPR5     ,STRING*50                    /.ﾒｯｾｰｼﾞ5    ./

    VAR ?PGNM     ,STRING*40                    /.ﾒｯｾｰｼﾞ1    ./
    VAR ?KEKA1    ,STRING*40                    /.      2    ./
    VAR ?KEKA2    ,STRING*40                    /.      3    ./
    VAR ?KEKA3    ,STRING*40                    /.      4    ./
    VAR ?KEKA4    ,STRING*40                    /.      5    ./
     /.ｾｷﾁｭｰ区分 1:資材,2:植物./
    VAR ?SEKKBN   ,STRING*1,VALUE-' '

/.##実行PG名称ｾｯﾄ##./
    ?PGNM := '流通ＢＭＳ　伝票発行'

/.###ﾌﾟﾛｸﾞﾗﾑ開始ﾒｯｾｰｼﾞ###./
    ?MSGX :=  '***   '  && ?PGMID  &&   ' START  ***'
    SNDMSG    ?MSGX,TO-XCTL

/.##ﾗｲﾌﾞﾗﾘﾘｽﾄ登録##./
    DEFLIBL   BMSFLIB/TOKELIB/TOKFLIB/TOKELIBO/TOKKLIB/TOKSOLIB/
              TOKMDLIB/TOKDLIB/TOKDTLIB

/.## ﾜｰｸｽﾃｰｼｮﾝ名取得##./
    ?WKSTN   :=  @ORGWS
    ?WS      :=  %STRING(?WKSTN)
    ?MSGX    :=  '## ﾜｰｸｽﾃｰｼｮﾝ名 = ' && ?WS
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##自倉庫／選択倉庫判定##./
CHK:
    IF      ?KBN   =    '3'   THEN
            ?FILNMD := 'BMSSDN'
    END
/.##倉庫ｺｰﾄﾞ取得##./
SKY1601B:

    ?STEP :=   'SKY1601B'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL      PGM-SKY1601B.TOKELIB,PARA-(?WS,?SOKCD,?DSOKCD)
    IF        @PGMEC    ^=   0   THEN
              ?KEKA4    :=   '倉庫ｺｰﾄﾞ取得'
              GOTO ABEND
    END
  /.?MSGX :=  '倉庫＝'  && ?SOKCD
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '代表倉庫＝'  && ?DSOKCD
    SNDMSG    ?MSGX,TO-XCTL     ./

/.##流通ＢＭＳ伝票発行指示入力##./
SBM0040I:

    ?STEP :=   'SBM0040I'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF  FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIBO
    OVRF     FILE-TOKMS2,TOFILE-TOKMS2.TOKFLIB
    OVRF     FILE-ZSOKMS1,TOFILE-ZSOKMS1.TOKFLIB
    OVRF     FILE-DENPGMS1,TOFILE-DENPGMS1.BMSFLIB
    CALL  PGM-SBM0040I.TOKELIBO,PARA-(?SOKCD,?DSOKCD
                                     ,?JDATE
                                     ,?JTIME
                                     ,?TORICD
                                     ,?SOKO
                                     ,?TENCD1
                                     ,?TENCD2
                                     ,?NOUDT1
                                     ,?NOUDT2
                                     ,?DENPTN
                                     ,?JYUN
                                     ,?PGCD
                                     ,?LIBCD)
    IF        @PGMEC    ^=   0    THEN
         IF   @PGMEC     =   4010 THEN
              SNDMSG MSG-'##取消終了##',TO-XCTL.@ORGPROF,JLOG-@YES
              GOTO RTN
         ELSE
              ?KEKA4    :=   '発行指示入力'
              GOTO ABEND
         END
    END

  /. TEST  ./
    ?MSGX :=  '受信日　　＝'  && ?JDATE
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '受信時刻　＝'  && ?JTIME
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '取引先　　＝'  && ?TORICD
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '出荷場所　＝'  && ?SOKO
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '開始店舗　＝'  && ?TENCD1
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '終了店舗　＝'  && ?TENCD2
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '開始納品日＝'  && ?NOUDT1
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '終了納品日＝'  && ?NOUDT2
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '伝票種類　＝'  && ?DENPTN
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '出力順　　＝'  && ?JYUN
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  'PG-ID     ＝'  && ?PGCD
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  'LIB-ID    ＝'  && ?LIBCD
    SNDMSG    ?MSGX,TO-XCTL

/.ビバホーム制御./
    IF    ?TORICD  =  '00038709'
      THEN
          GOTO SBM0050B
    END

/.##ﾌｧｲﾙ名称取得##./
FILNMCHG:
    ?FILNM    :=    ?FILNMD && ?SOKO
    ?FILID    :=    %NAME(?FILNM)
    ?LIBID    :=    %NAME(?LIBNM)
    ?DENPYO   :=    %NCAT(?FILID,?LIBID)
    ?DENPYON  :=    %STRING(?DENPYO)
    ?MSGX     :=    '## 伝票ﾌｧｲﾙ名 = ' && ?DENPYON && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
                                      /.##SORT順 FILE##./
    ?FILNM    :=    ?FILNMO && ?SOKO
    ?FILID    :=    %NAME(?FILNM)
    ?LIBID    :=    %NAME(?LIBNM)
    ?BMSDT    :=    %NCAT(?FILID,?LIBID)
    ?BMSDTN   :=    %STRING(?BMSDT)
    ?MSGX     :=    '## 中間F(PF)  = ' && ?BMSDTN  && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
                                      /.##SORT順 FILE##./
    ?FILNM    :=    ?FILNMO && ?SOKO && ?KEYA
    ?FILID    :=    %NAME(?FILNM)
    ?LIBID    :=    %NAME(?LIBNM)
    ?BMSDT1   :=    %NCAT(?FILID,?LIBID)
    ?BMSDT1N  :=    %STRING(?BMSDT1)
    ?MSGX     :=    '## 中間F(LF1) = ' && ?BMSDT1N && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
                                      /.##SORT順 FILE##./
    ?FILNM    :=    ?FILNMO && ?SOKO && ?KEYB
    ?FILID    :=    %NAME(?FILNM)
    ?LIBID    :=    %NAME(?LIBNM)
    ?BMSDT2   :=    %NCAT(?FILID,?LIBID)
    ?BMSDT2N  :=    %STRING(?BMSDT2)
    ?MSGX     :=    '## 中間F(LF2) = ' && ?BMSDT2N && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
                                      /.##SORT順 FILE##./
    ?FILNM    :=    ?FILNMO && ?SOKO  && ?KEYC
    ?FILID    :=    %NAME(?FILNM)
    ?LIBID    :=    %NAME(?LIBNM)
    ?BMSDT3   :=    %NCAT(?FILID,?LIBID)
    ?BMSDT3N  :=    %STRING(?BMSDT3)
    ?MSGX     :=    '## 中間F(LF3) = ' && ?BMSDT3N && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES

/.##流通ＢＭＳ伝票発行データ抽出##./
SBM0050B:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRF      FILE-BMSHACL1,TOFILE-BMSHACL1.BMSFLIB
    OVRF      FILE-SHTDENLA,TOFILE-SHTDENLA.TOKFLIB
    OVRF      FILE-BMSDENF,TOFILE-?DENPYO
   /. 2013/01/30 取引先別に制御 ./
    CASE ?TORICD OF
      #'00013020'# /. バロー ./
       /.CALL PGM-SBM0052B.TOKELIBO,PARA-(?JDATE 20240311 ./
         CALL PGM-SBM0062B.TOKSOLIB,PARA-(?JDATE
                                          ,?JTIME
                                          ,?TORICD
                                          ,?SOKO
                                          ,?TENCD1,?TENCD2
                                          ,?NOUDT1,?NOUDT2)
         IF   @PGMEC     =   4010 THEN
              SNDMSG MSG-'##対象無し##',TO-XCTL.@ORGPROF,JLOG-@YES
              GOTO RTN
         END
         IF   @PGMEC    ^=   0    THEN
              ?KEKA4    :=   '流通ＢＭＳ伝票発行データ抽出'
              GOTO ABEND
         END
      #'00013052'# /. バロー ./
       /.CALL PGM-SBM0052B.TOKELIBO,PARA-(?JDATE  20240311 ./
         CALL PGM-SBM0062B.TOKSOLIB,PARA-(?JDATE
                                          ,?JTIME
                                          ,?TORICD
                                          ,?SOKO
                                          ,?TENCD1,?TENCD2
                                          ,?NOUDT1,?NOUDT2)
         IF   @PGMEC     =   4010 THEN
              SNDMSG MSG-'##対象無し##',TO-XCTL.@ORGPROF,JLOG-@YES
              GOTO RTN
         END
         IF   @PGMEC    ^=   0    THEN
              ?KEKA4    :=   '流通ＢＭＳ伝票発行データ抽出'
              GOTO ABEND
         END
      #'00751225'# /. アレンザＨＤ（ダイユーエイト）./
         CALL PGM-SBM0062D.TOKSOLIB,PARA-(?JDATE
                                          ,?JTIME
                                          ,?TORICD
                                          ,?SOKO
                                          ,?TENCD1,?TENCD2
                                          ,?NOUDT1,?NOUDT2)
         IF   @PGMEC     =   4010 THEN
              SNDMSG MSG-'##対象無し##',TO-XCTL.@ORGPROF,JLOG-@YES
              GOTO RTN
         END
         IF   @PGMEC    ^=   0    THEN
              ?KEKA4    :=   '流通ＢＭＳ伝票発行データ抽出'
              GOTO ABEND
         END
      #'00781225'# /. アレンザＨＤ（日敷　　　　　）./
         CALL PGM-SBM0062D.TOKSOLIB,PARA-(?JDATE
                                          ,?JTIME
                                          ,?TORICD
                                          ,?SOKO
                                          ,?TENCD1,?TENCD2
                                          ,?NOUDT1,?NOUDT2)
         IF   @PGMEC     =   4010 THEN
              SNDMSG MSG-'##対象無し##',TO-XCTL.@ORGPROF,JLOG-@YES
              GOTO RTN
         END
         IF   @PGMEC    ^=   0    THEN
              ?KEKA4    :=   '流通ＢＭＳ伝票発行データ抽出'
              GOTO ABEND
         END
      #'00761995'# /. アレンザＨＤ（タイム　　　　）./
         CALL PGM-SBM0062D.TOKSOLIB,PARA-(?JDATE
                                          ,?JTIME
                                          ,?TORICD
                                          ,?SOKO
                                          ,?TENCD1,?TENCD2
                                          ,?NOUDT1,?NOUDT2)
         IF   @PGMEC     =   4010 THEN
              SNDMSG MSG-'##対象無し##',TO-XCTL.@ORGPROF,JLOG-@YES
              GOTO RTN
         END
         IF   @PGMEC    ^=   0    THEN
              ?KEKA4    :=   '流通ＢＭＳ伝票発行データ抽出'
              GOTO ABEND
         END
      #'00271934'# /. コメリ ./
       /.CALL PGM-SBM0053B.TOKELIBO,PARA-(?JDATE  20221025./
         CALL PGM-SBM0063B.TOKSOLIB,PARA-(?JDATE
                                          ,?JTIME
                                          ,?TORICD
                                          ,?SOKO
                                          ,?TENCD1,?TENCD2
                                          ,?NOUDT1,?NOUDT2)
         IF   @PGMEC     =   4010 THEN
              SNDMSG MSG-'##対象無し##',TO-XCTL.@ORGPROF,JLOG-@YES
              GOTO RTN
         END
         IF   @PGMEC    ^=   0    THEN
              ?KEKA4    :=   '流通ＢＭＳ伝票発行データ抽出'
              GOTO ABEND
         END
      #'00321717'# /. セキチュー資材 ./
         CALL PGM-SBM0054B.TOKSOLIB,PARA-(?JDATE
                                          ,?JTIME
                                          ,?TORICD
                                          ,?SOKO
                                          ,?TENCD1,?TENCD2
                                          ,?NOUDT1,?NOUDT2)
         IF   @PGMEC    ^=   0    THEN
              ?KEKA4    :=   '流通ＢＭＳ伝票発行データ抽出資材'
              GOTO ABEND
         END
      #'00926061'# /. セキチュー植物 ./
         CALL PGM-SBM0054B.TOKSOLIB,PARA-(?JDATE
                                          ,?JTIME
                                          ,?TORICD
                                          ,?SOKO
                                          ,?TENCD1,?TENCD2
                                          ,?NOUDT1,?NOUDT2)
         IF   @PGMEC    ^=   0    THEN
              ?KEKA4    :=   '流通ＢＭＳ伝票発行データ抽出植物'
              GOTO ABEND
         END
      #'00116501'# /. ムサシ ./
         CALL PGM-SBM0056B.TOKSOLIB,PARA-(?JDATE
                                          ,?JTIME
                                          ,?TORICD
                                          ,?SOKO
                                          ,?TENCD1,?TENCD2
                                          ,?NOUDT1,?NOUDT2)
         IF   @PGMEC    ^=   0    THEN
              ?KEKA4    :=   '流通ＢＭＳ伝票発行データ抽出'
              GOTO ABEND
         END
      #'00001041'# /. 山新　 ./
         CALL PGM-SBM0055B.TOKSOLIB,PARA-(?JDATE
                                          ,?JTIME
                                          ,?TORICD
                                          ,?SOKO
                                          ,?TENCD1,?TENCD2
                                          ,?NOUDT1,?NOUDT2)
         IF   @PGMEC    ^=   0    THEN
              ?KEKA4    :=   '流通ＢＭＳ伝票発行データ抽出'
              GOTO ABEND
         END
      #'00005116'# /. ジュンテンドー　 ./
         CALL PGM-SBM0058B.TOKSOLIB,PARA-(?JDATE
                                          ,?JTIME
                                          ,?TORICD
                                          ,?SOKO
                                          ,?TENCD1,?TENCD2
                                          ,?NOUDT1,?NOUDT2)
         IF   @PGMEC    ^=   0    THEN
              ?KEKA4    :=   '流通ＢＭＳ伝票発行データ抽出'
              GOTO ABEND
         END
      #'00038709'# /. ビバホーム　 ./
         OVRF FILE-WKDENVH,TOFILE-WKDENVH.BMSFLIB
         CALL PGM-SBM0059B.TOKSOLIB,PARA-(?JDATE
                                          ,?JTIME
                                          ,?TORICD
                                          ,?SOKO
                                          ,?TENCD1,?TENCD2
                                          ,?NOUDT1,?NOUDT2)
         IF   @PGMEC    ^=   0    THEN
              ?KEKA4    :=   '流通ＢＭＳ伝票発行データ抽出'
              GOTO ABEND
         END
         OVRF FILE-WKDENVH1,TOFILE-WKDENVH1.BMSFLIB
         CALL PGM-SBM0060B.TOKSOLIB
         IF   @PGMEC    ^=   0    THEN
              ?KEKA4    :=   '流通ＢＭＳ伝票行数カウント'
              GOTO ABEND
         END
      #'00003043'# /. ジョイフルエーケー　 ./
         CALL PGM-SSY5330B.TOKSOLIB,PARA-(?JDATE
                                          ,?JTIME
                                          ,?TORICD
                                          ,?SOKO
                                          ,?TENCD1,?TENCD2
                                          ,?NOUDT1,?NOUDT2)
         IF   @PGMEC    ^=   0    THEN
              ?KEKA4    :=   '流通ＢＭＳ伝票発行データ抽出'
              GOTO ABEND
         END
      #'00000616'# /. ハンズマン ./
         CALL PGM-SSY4536B.TOKSOLIB,PARA-(?JDATE
                                          ,?JTIME
                                          ,?TORICD
                                          ,?SOKO
                                          ,?TENCD1,?TENCD2
                                          ,?NOUDT1,?NOUDT2)
         IF   @PGMEC    ^=   0    THEN
              ?KEKA4    :=   '流通ＢＭＳ伝票発行データ抽出'
              GOTO ABEND
         END

      ELSE
         CALL PGM-SBM0050B.TOKELIBO,PARA-(?JDATE
                                          ,?JTIME
                                          ,?TORICD
                                          ,?SOKO
                                          ,?TENCD1,?TENCD2
                                          ,?NOUDT1,?NOUDT2)
         IF   @PGMEC     =   4010 THEN
              SNDMSG MSG-'##対象無し##',TO-XCTL.@ORGPROF,JLOG-@YES
              GOTO RTN
         END
         IF   @PGMEC    ^=   0    THEN
              ?KEKA4    :=   '流通ＢＭＳ伝票発行データ抽出'
              GOTO ABEND
         END
    END

    ?MSGX :=  '倉庫＝'  && ?SOKO
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '出力順＝'  && ?JYUN
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '受信日＝'  && ?JDATE
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '受信時刻＝'  && ?JTIME
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '取引先＝'  && ?TORICD
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  'PG-ID＝'  && ?PGCD
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  'LIB-ID＝'  && ?LIBCD
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '開始納品日＝'  && ?NOUDT1
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '終了納品日＝'  && ?NOUDT2
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '開始店舗＝'  && ?TENCD1
    SNDMSG    ?MSGX,TO-XCTL
    ?MSGX :=  '終了店舗＝'  && ?TENCD2
    SNDMSG    ?MSGX,TO-XCTL

/.ビバホーム制御./
    IF    ?TORICD  =  '00038709'
      THEN
          GOTO CHGPRT
    END
/.##オンラインデータ排他処理##./
ASSIGN01:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    ASSIGN FILE-?BMSDT!@XCL
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4    :=   'オンラインデータ排他処理'
              GOTO ASSERR
    END

/.##オンラインデータ物理Ｆへコピー##./
PSETPF:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    SETPF FILE-?DENPYO,TOFILE-?BMSDT,ADD-@NO,ACTCHK-@NO
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4    :=   'オンラインデータ複写処理'
              GOTO ASSERR
    END
    DLTOVRF FILE-BMSDENF/DSPF

/.##出力順別抽出処理##./
SBM0051B:

    ?STEP :=   %LAST(LABEL)
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIBO
    OVRF      FILE-BMSDT1,TOFILE-?BMSDT1
    OVRF      FILE-BMSDT2,TOFILE-?BMSDT2
    OVRF      FILE-BMSDT3,TOFILE-?BMSDT3
    OVRF      FILE-BMSDENF,TOFILE-?DENPYO
    CALL      PGM-SBM0051B.TOKELIBO,PARA-(?JYUN
                                         ,?TENCD1,?TENCD2
                                         ,?NOUDT1,?NOUDT2)
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4    :=   '出力順別抽出処理'
              GOTO ABEND END

    IF  ?TORICD = '00003043' THEN
         OVRF      FILE-BMSDT1,TOFILE-?BMSDT1
         OVRF      FILE-BMSDT2,TOFILE-?BMSDT2
         OVRF      FILE-BMSDT3,TOFILE-?BMSDT3
         OVRF      FILE-BMSDENF,TOFILE-?DENPYO
         CALL      PGM-SBM0051J.TOKSOLIB,PARA-(?JYUN
                                         ,?TENCD1,?TENCD2
                                         ,?NOUDT1,?NOUDT2)
         IF        @PGMEC    ^=   0    THEN
                   ?KEKA4    :=   '出力順別抽出処理（ＡＫ）'
                   GOTO ABEND END
    END

CHGPRT:
/.##流通ＢＭＳ伝票発行制御##./
/.## ﾌﾟﾘﾝﾀｰ切替   未確定      ##./
/.----- IF  ?KBN   =    '1'   THEN
            CHGCMVAR '@OUTQN',XOUTQ1
        END
        IF  ?KBN   =    '2'   THEN
            CHGCMVAR '@OUTQN',XOUTQ1
        END
        IF  ?KBN   =    '3'   THEN
            IF  ?WS  =  'WKSTNH83'  THEN
                CHGCMVAR '@OUTQN',XXPXLX83
            END
        END    -------------------------- ./

/.2013/01/30　取引先別制御./
/.  IF    ?TORICD  =  '00013020'
      THEN
          GOTO OUTPUT02
      ELSE
          GOTO OUTPUT01
    END           ./
    CASE ?TORICD OF
      #'00013020'# /. バロー ./
         GOTO OUTPUT02
      #'00013052'# /. バロー ./
         GOTO OUTPUT02
      #'00751225'# /. アレンザＨＤ（ダイユーエイト）./
         GOTO OUTPUT02
      #'00761995'# /. アレンザＨＤ（タイム　　　　）./
         GOTO OUTPUT02
      #'00781225'# /. アレンザＨＤ（日敷　　　　　）./
         GOTO OUTPUT02
      #'00271934'# /. コメリ ./
         GOTO OUTPUT03
      #'00321717'# /. セキチュー資材 ./
         ?SEKKBN := '1'
         GOTO OUTPUT04
      #'00926061'# /. セキチュー植物 ./
         ?SEKKBN := '2'
         GOTO OUTPUT04
      #'00116501'# /. ムサシ ./
         IF  ?DENPTN  =  '1'  THEN
              GOTO OUTPUT05
         ELSE
              GOTO OUTPUT06
         END
      #'00001041'# /. 山新　 ./
         GOTO OUTPUT07
      #'00005116'# /. ジュンテンドー　 ./
         GOTO OUTPUT08
      #'00038709'# /. ビバホーム　./
         GOTO OUTPUT10
      #'00003043'# /. ジョイフルエーケー　./
         GOTO OUTPUT11
      ELSE
         GOTO OUTPUT01
    END

/.##伝票発行処理##./
OUTPUT01:

    ?STEP :=  ?PGCD
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    ?PGNAME   := %NAME(?PGCD )
    ?LIBNAME  := %NAME(?LIBCD)
    ?PGC      := %NCAT(?PGNAME,?LIBNAME)
    ?PGN      := %STRING(?PGC)
    OVRDSPF   FILE-DSPFILE,TOFILE-DSPF.TOKELIBO,
              MEDLIB-TOKELIBO
    OVRF      FILE-BMSDENF,TOFILE-?DENPYO
    CALL      PGM-?PGC
    IF   @PGMEC     =   4010 THEN
         SNDMSG MSG-'##対象無し##',TO-XCTL.@ORGPROF,JLOG-@YES
         GOTO RTN
    END
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4    :=   '伝票発行処理'
              GOTO ABEND
    ELSE
              GOTO RTN
    END

/.##バロー納品明細データ作成処理##./
OUTPUT02:

    ?STEP :=  ?PGCD
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    ?PGNAME   := %NAME(?PGCD )
    ?LIBNAME  := %NAME(?LIBCD)
    ?PGC      := %NCAT(?PGNAME,?LIBNAME)
    ?PGN      := %STRING(?PGC)
    ?MSGX :=  'SORTF-IN   '
    SNDMSG    ?MSGX,TO-XCTL
    /. データＳＯＲＴ ./
 /. SORTF  INFILE-?DENPYO,
           OUTFILE-?DENPYO,
           WORKFILE-BMSDENST.BMSFLIB,
           KEY-'F013!A',
           KEY1-'F328!A',
           KEY2-'F346!A',
           KEY3-'F331!A',
           KEY4-'F304!A',
           KEY5-'F342!A',
           KEY6-'F308!A',
           KEY7-'F302!A',
           KEY8-'F402!A',
           SELECT-'F013!EQ!#00013020',
           RCDL-@DSP        ./
    SORTF  INFILE-?DENPYO,
           OUTFILE-?DENPYO,
           WORKFILE-BMSDENST.BMSFLIB,
           KEY-'F013!A',
           KEY1-'F328!A',
           KEY2-'F346!A',
           KEY3-'F331!A',
           KEY4-'F304!A',
           KEY5-'F342!A',
           KEY6-'F308!A',
           KEY7-'F302!A',
           KEY8-'F402!A',
           RCDL-@DSP
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4    :=   '伝票データＳＯＲＴ処理'
              GOTO ABEND
    END
    ?MSGX :=  'SORTF-OUT  '
    SNDMSG    ?MSGX,TO-XCTL

    /. 処理前データクリア ./
    CLRFILE   FILE-BRNOMEIF.BMSFLIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4    :=   '処理前データクリア処理'
              GOTO ABEND
    END

    /. 納品明細データ作成 ./
    OVRF      FILE-BMSDENF,TOFILE-?DENPYO
    OVRF      FILE-BRNOMEI1,TOFILE-BRNOMEI1.BMSFLIB
    OVRF      FILE-TENMS1,TOFILE-TENMS1.TOKFLIB
    OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
    CALL      PGM-?PGC
    IF   @PGMEC     =   4010 THEN
         SNDMSG MSG-'##対象無し##',TO-XCTL.@ORGPROF,JLOG-@YES
         GOTO RTN
    END
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4    :=   '納品明細データ作成処理'
              GOTO ABEND
    END

    /. 納品明細データ複写 ./
    CNVFILE   FILE-BRNOMEIF.BMSFLIB,
              TOFILE-BRNOMESF.BMSFLIB,
              ADD-@NO,BF-1
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4    :=   '納品明細データ複写処理'
              GOTO ABEND
    END

    /. 納品明細データＰＣ転送 ./
    IF  ?TORICD  =  '00013020'  THEN
         FIMPORT   FILE-BRNOMESF.BMSFLIB,
                   TYPE-@FILE,
                   PARA-BRNOMEIF,
                   UNIT-1
         IF        @PGMEC    ^=   0    THEN
                   ?KEKA4    :=   '納品明細ＤＴ転送（バロー１）'
                   GOTO ABEND
         ELSE
                   GOTO RTN
         END
    END
    IF  ?TORICD  =  '00013052'  THEN
         FIMPORT   FILE-BRNOMESF.BMSFLIB,
                   TYPE-@FILE,
                   PARA-BRNOMEIF,
                   UNIT-1
         IF        @PGMEC    ^=   0    THEN
                   ?KEKA4    :=   '納品明細ＤＴ転送（バロー２）'
                   GOTO ABEND
         ELSE
                   GOTO RTN
         END
    END
    IF  ?TORICD  =  '00751225'  THEN
         FIMPORT   FILE-BRNOMESF.BMSFLIB,
                   TYPE-@FILE,
                   PARA-BRNOMEIF,
                   UNIT-2
         IF        @PGMEC    ^=   0    THEN
                   ?KEKA4    :=   '納品明細ＤＴ転送（ダイユー）'
                   GOTO ABEND
         ELSE
                   GOTO RTN
         END
    END
    IF  ?TORICD  =  '00781225'  THEN
         FIMPORT   FILE-BRNOMESF.BMSFLIB,
                   TYPE-@FILE,
                   PARA-BRNOMEIF,
                   UNIT-3
         IF        @PGMEC    ^=   0    THEN
                   ?KEKA4    :=   '納品明細ＤＴ転送（日敷　　）'
                   GOTO ABEND
         ELSE
                   GOTO RTN
         END
    END
    IF  ?TORICD  =  '00761995'  THEN
         FIMPORT   FILE-BRNOMESF.BMSFLIB,
                   TYPE-@FILE,
                   PARA-BRNOMEIF,
                   UNIT-4
         IF        @PGMEC    ^=   0    THEN
                   ?KEKA4    :=   '納品明細ＤＴ転送（タイム　）'
                   GOTO ABEND
         ELSE
                   GOTO RTN
         END
    END

/.##コメリ発注明細データ作成処理##./
OUTPUT03:

    ?STEP :=  ?PGCD
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    ?PGNAME   := %NAME(?PGCD )
    ?LIBNAME  := %NAME(?LIBCD)
    ?PGC      := %NCAT(?PGNAME,?LIBNAME)
    ?PGN      := %STRING(?PGC)
    ?MSGX :=  'SORTF-IN   '
    SNDMSG    ?MSGX,TO-XCTL
    /. データＳＯＲＴ ./
    SORTF  INFILE-?DENPYO,
           OUTFILE-?DENPYO,
           WORKFILE-BMSDENXX.BMSFLIB,
           KEY-'F210!A',
           KEY1-'F304!A',
           KEY2-'F344!A',
           KEY3-'F303!A',
           KEY4-'F323!A',
           KEY5-'F302!A',
           KEY6-'F402!A',
           SELECT-'F013!EQ!#00271934',
           RCDL-@DSP
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4    :=   '伝票データＳＯＲＴ処理'
              GOTO ABEND
    END
    ?MSGX :=  'SORTF-OUT  '
    SNDMSG    ?MSGX,TO-XCTL

    /. 処理前データクリア ./
    CLRFILE   FILE-KMRMEIF.BMSFLIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4    :=   '処理前データクリア処理'
              GOTO ABEND
    END

    /. 発注明細データ作成 ./
    OVRF      FILE-BMSDENF,TOFILE-?DENPYO
    OVRF      FILE-KMRMEIF,TOFILE-KMRMEIF.BMSFLIB
    CALL      PGM-?PGC,PARA-('0')
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4    :=   '納品明細データ作成処理'
              GOTO RTN
    END

    /. 発注明細データ複写 ./
    CNVFILE   FILE-KMRMEIF.BMSFLIB,
              TOFILE-KMRMEISF.BMSFLIB,
              ADD-@NO,BF-1
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4    :=   '発注明細データ複写処理'
              GOTO ABEND
    END

    /. 発注明細データＰＣ転送 ./
    FIMPORT   FILE-KMRMEISF.BMSFLIB,
              TYPE-@FILE,
              PARA-KMRMEIF,
              UNIT-1
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4    :=   '発注明細データＰＣ転送処理'
              GOTO ABEND
    END
    /. データＳＯＲＴ ./
    SORTF  INFILE-?DENPYO,
           OUTFILE-?DENPYO,
           WORKFILE-BMSDENXX.BMSFLIB,
           KEY-'F210!A',
           KEY1-'F304!A',
           KEY2-'F344!A',
           KEY3-'F303!A',
           KEY4-'F323!A',
           KEY5-'F403!A',
           SELECT-'F013!EQ!#00271934',
           RCDL-@DSP
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4    :=   '伝票データＳＯＲＴ処理'
              GOTO ABEND
    END
    ?MSGX :=  'SORTF-OUT  '
    SNDMSG    ?MSGX,TO-XCTL

    /. 処理前データクリア ./
    CLRFILE   FILE-KMRMEIF.BMSFLIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4    :=   '処理前データクリア処理'
              GOTO ABEND
    END

    /. 商品識別シールデータ作成 ./
    OVRF      FILE-BMSDENF,TOFILE-?DENPYO
    OVRF      FILE-KMRMEIF,TOFILE-KMRMEIF.BMSFLIB
    CALL      PGM-?PGC,PARA-('1')
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4    :=   '商品識別シール作成処理'
              GOTO RTN
    END

    /. 商品識別シールデータ複写 ./
    CNVFILE   FILE-KMRMEIF.BMSFLIB,
              TOFILE-KMRMEISF.BMSFLIB,
              ADD-@NO,BF-1
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4    :=   '商品識別シール複写処理'
              GOTO ABEND
    END

    /. 商品識別シールＰＣ転送 ./
    FIMPORT   FILE-KMRMEISF.BMSFLIB,
              TYPE-@FILE,
              PARA-KMRMEIF,
              UNIT-2
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4    :=   '商品識別シールＰＣ転送処理'
              GOTO ABEND
    ELSE
              GOTO RTN
    END

/.##セキチュー納品明細データ作成処理##./
OUTPUT04:

    ?STEP :=  ?PGCD
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    ?PGNAME   := %NAME(?PGCD )
    ?LIBNAME  := %NAME(?LIBCD)
    ?PGC      := %NCAT(?PGNAME,?LIBNAME)
    ?PGN      := %STRING(?PGC)
    ?MSGX :=  'SORTF-IN   '
    SNDMSG    ?MSGX,TO-XCTL
    /. データＳＯＲＴ ./
    SORTF  INFILE-?DENPYO,
           OUTFILE-?DENPYO,
           RCD-50,
    /.     KEY-'F013!A',               取引先ＣＤ./
    /.     KEY1-'F304!A',              直接納品先ＣＤ./
    /.     KEY2-'F308!A',              最終納品先ＣＤ./
    /.     KEY3-'F112!A',              作成日時./
    /.     KEY4-'F346!A',              最終納品先納品日./
    /.     KEY5-'F342!A',              商品分類（大）部門ＣＤ./
    /.     KEY6-'F331!A',              便ＮＯ./
    /.     KEY7-'F357!A',              処理種別./
    /.     KEY8-'F302!A',              取引番号./
    /.     KEY9-'F402!A',              行番号./
    /.     RCDL-@DSP                         ./
           KEY-'F013!A',             /.取引先ＣＤ./
           KEY1-'F304!A',            /.直接納品先ＣＤ./
           KEY2-'F308!A',            /.最終納品先ＣＤ./
           KEY3-'F112!A',            /.作成日時./
           KEY4-'F344!A',            /.発注日./
           KEY5-'F346!A',            /.最終納品先納品日./
           KEY6-'F342!A',            /.商品分類（大）部門ＣＤ./
           KEY7-'F331!A',            /.便ＮＯ./
           KEY8-'F302!A',            /.取引番号./
           KEY9-'F402!A',            /.行番号./
           RCDL-@DSP
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4    :=   '伝票データＳＯＲＴ処理'
              GOTO ABEND
    END
    ?MSGX :=  'SORTF-OUT  '
    SNDMSG    ?MSGX,TO-XCTL

    /. 処理前データクリア ./
    CLRFILE   FILE-SKNOMEIF.BMSFLIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4    :=   '処理前データクリア処理'
              GOTO ABEND
    END

    IF  ?DENPTN  =  '1'  THEN    /.伝票種別＝１の時./
        /. 納品明細データ作成 ./
        OVRF      FILE-BMSDENF,TOFILE-?DENPYO
        OVRF      FILE-SKNOMEI1,TOFILE-SKNOMEI1.BMSFLIB
        OVRF      FILE-TENMS1,TOFILE-TENMS1.TOKFLIB
        OVRF      FILE-JYOKEN1,TOFILE-JYOKEN1.TOKFLIB
        CALL      PGM-?PGC
        IF        @PGMEC    ^=   0    THEN
                  ?KEKA4    :=   '納品明細データ作成処理'
                  GOTO ABEND
        END

        /. 納品明細データ複写 ./
        CNVFILE   FILE-SKNOMEIF.BMSFLIB,
                  TOFILE-SKNOMESF.BMSFLIB,
                  ADD-@NO,BF-1
         IF        @PGMEC    ^=   0    THEN
                   ?KEKA4    :=   '納品明細データ複写処理'
                   GOTO ABEND
         END

         /. 納品明細データＰＣ転送 ./
         FIMPORT   FILE-SKNOMESF.BMSFLIB,
                   TYPE-@FILE,
                   PARA-SKNOMEIF,
                   UNIT-1
         IF        @PGMEC    ^=   0    THEN
                   ?KEKA4    :=   '納品明細データＰＣ転送処理'
                   GOTO ABEND
         ELSE
                   GOTO RTN
         END
    ELSE
        /. 納品明細データ作成 ./
        OVRF      FILE-BMSDENF,TOFILE-?DENPYO
        OVRPRTF FILE-DSPF,TOFILE-DSPF.TOKELIBO,SEP-@NO,
                MEDLIB-TOKELIBO
        CALL      PGM-?PGC,PARA-(?SEKKBN)
        IF        @PGMEC    ^=   0    THEN
                  ?KEKA4    :=   'ＴＡ_伝票発行処理'
                  GOTO ABEND
        END
    END

/.##ムサシ納品明細書発行データ作成##./
OUTPUT05:

    ?STEP :=  'OUTPUT05'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    ?PGNAME   := %NAME(?PGCD )
    ?LIBNAME  := %NAME(?LIBCD)
    ?PGC      := %NCAT(?PGNAME,?LIBNAME)
    ?PGN      := %STRING(?PGC)
    ?MSGX :=  'SORTF-IN   '
    SNDMSG    ?MSGX,TO-XCTL
    /. データＳＯＲＴ ./
    SORTF  INFILE-?DENPYO,
           OUTFILE-?DENPYO,
           RCD-50,
           KEY-'F323!A',             /.取引先ＣＤ./
           KEY1-'F304!A',            /.直接納品先ＣＤ./
           KEY2-'F342!A',            /.商品分類（大）部門ＣＤ./
           KEY3-'F308!A',            /.最終納品先ＣＤ./
           KEY4-'F112!A',            /.作成日時./
           KEY5-'F344!A',            /.発注日./
           KEY6-'F346!A',            /.最終納品先納品日./
           KEY7-'F331!A',            /.便ＮＯ./
           KEY8-'F302!A',            /.取引番号./
           KEY9-'F402!A',            /.行番号./
           RCDL-@DSP
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4    :=   '伝票データＳＯＲＴ処理'
              GOTO ABEND
    END
    ?MSGX :=  'SORTF-OUT  '
    SNDMSG    ?MSGX,TO-XCTL

    /. 処理前データクリア ./
    CLRFILE   FILE-ARNOMEIF.BMSFLIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4    :=   '処理前データクリア処理'
              GOTO ABEND
    END

        /. 納品明細データ作成 ./
        OVRF      FILE-WKDENAR,TOFILE-?DENPYO
        OVRF      FILE-ARNOMEI1,TOFILE-ARNOMEI1.BMSFLIB
        CALL      PGM-?PGC
        IF        @PGMEC    ^=   0    THEN
                  ?KEKA4    :=   '納品明細データ作成処理'
                  GOTO ABEND
        END

        /. 納品明細データ複写 ./
        CNVFILE   FILE-ARNOMEIF.BMSFLIB,
                  TOFILE-ARNOMESF.BMSFLIB,
                  ADD-@NO,BF-1
         IF        @PGMEC    ^=   0    THEN
                   ?KEKA4    :=   '納品明細データ複写処理'
                   GOTO ABEND
         END

         /. 納品明細データＰＣ転送 ./
         FIMPORT   FILE-ARNOMESF.BMSFLIB,
                   TYPE-@FILE,
                   PARA-ARNOMEIF,
                   UNIT-1
         IF        @PGMEC    ^=   0    THEN
                   ?KEKA4    :=   '納品明細データＰＣ転送処理'
                   GOTO ABEND
         ELSE
                   GOTO RTN
         END

/.##ムサシ仕入伝票発行データ作成##./
OUTPUT06:

    ?STEP :=  'OUTPUT06'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    ?PGNAME   := %NAME(?PGCD )
    ?LIBNAME  := %NAME(?LIBCD)
    ?PGC      := %NCAT(?PGNAME,?LIBNAME)
    ?PGN      := %STRING(?PGC)
    ?MSGX :=  'SORTF-IN   '
    SNDMSG    ?MSGX,TO-XCTL
    /. データＳＯＲＴ ./
    SORTF  INFILE-?DENPYO,
           OUTFILE-?DENPYO,
           RCD-50,
           KEY-'F323!A',             /.取引先ＣＤ./
           KEY1-'F304!A',            /.直接納品先ＣＤ./
           KEY2-'F344!A',            /.発注日./
           KEY3-'F345!A',            /.直接納品先納品日./
           KEY4-'F302!A',            /.伝票番号./
           KEY5-'F402!A',            /.行番号./
           RCDL-@DSP
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4    :=   '伝票データＳＯＲＴ処理'
              GOTO ABEND
    END
    ?MSGX :=  'SORTF-OUT  '
    SNDMSG    ?MSGX,TO-XCTL

    /. 処理前データクリア ./
    CLRFILE   FILE-ARSIDENF.BMSFLIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4    :=   '処理前データクリア処理'
              GOTO ABEND
    END

        /. 仕入伝票データ作成 ./
        OVRF      FILE-BMSDENF,TOFILE-?DENPYO
        OVRF      FILE-ARSIDEN1,TOFILE-ARSIDEN1.BMSFLIB
        CALL      PGM-?PGC
        IF        @PGMEC    ^=   0    THEN
                  ?KEKA4    :=   '仕入伝票データ作成処理'
                  GOTO ABEND
        END

        /. 仕入伝票データ複写 ./
        CNVFILE   FILE-ARSIDENF.BMSFLIB,
                  TOFILE-ARDENSF.BMSFLIB,
                  ADD-@NO,BF-1
         IF        @PGMEC    ^=   0    THEN
                   ?KEKA4    :=   '仕入伝票データ複写処理'
                   GOTO ABEND
         END

         /. 仕入伝票データＰＣ転送 ./
         FIMPORT   FILE-ARDENSF.BMSFLIB,
                   TYPE-@FILE,
                   PARA-ARDENF,
                   UNIT-1
         IF        @PGMEC    ^=   0    THEN
                   ?KEKA4    :=   '仕入伝票データＰＣ転送処理'
                   GOTO ABEND
         ELSE
                   GOTO RTN
         END

/.##山新納品明細書発行データ作成##./
OUTPUT07:

    ?STEP :=  'OUTPUT07'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    ?PGNAME   := %NAME(?PGCD )
    ?LIBNAME  := %NAME(?LIBCD)
    ?PGC      := %NCAT(?PGNAME,?LIBNAME)
    ?PGN      := %STRING(?PGC)
    ?MSGX :=  'SORTF-IN   '
    SNDMSG    ?MSGX,TO-XCTL
    ?DENPYON  :=    %STRING(?DENPYO)
    ?MSGX     :=    '## 伝票ﾌｧｲﾙ名 = ' && ?DENPYON && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    /. データＳＯＲＴ ./
    SORTF  INFILE-?DENPYO,
           OUTFILE-?DENPYO,
           RCD-50,
           KEY-'F323!A',             /.取引先ＣＤ./
           KEY1-'F308!A',            /.最終納品先ＣＤ./
           KEY2-'F112!A',            /.作成日時./
           KEY3-'F344!A',            /.発注日./
           KEY4-'F346!A',            /.最終納品先納品日./
           KEY5-'F342!A',            /.商品分類（大）部門ＣＤ./
           KEY6-'F357!A',            /.伝票区分./
           KEY7-'F302!A',            /.取引番号./
           KEY8-'F402!A',            /.行番号./
           RCDL-@DSP
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4    :=   '伝票データＳＯＲＴ処理'
              GOTO ABEND
    END
    ?MSGX :=  'SORTF-OUT  '
    SNDMSG    ?MSGX,TO-XCTL

    /. 処理前データクリア ./
    CLRFILE   FILE-YMNOMEIF.BMSFLIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4    :=   '処理前データクリア処理'
              GOTO ABEND
    END

        /. 納品明細データ作成 ./
        OVRF      FILE-BMSDENF,TOFILE-?DENPYO
        OVRF      FILE-YMNOMEI1,TOFILE-YMNOMEI1.BMSFLIB
        CALL      PGM-?PGC
        IF        @PGMEC    ^=   0    THEN
                  ?KEKA4    :=   '納品明細データ作成処理'
                  GOTO ABEND
        END

        /. 納品明細データ複写 ./
        CNVFILE   FILE-YMNOMEIF.BMSFLIB,
                  TOFILE-YMNOMESF.BMSFLIB,
                  ADD-@NO,BF-1
         IF        @PGMEC    ^=   0    THEN
                   ?KEKA4    :=   '納品明細データ複写処理'
                   GOTO ABEND
         END

         /. 納品明細データＰＣ転送 ./
         FIMPORT   FILE-YMNOMESF.BMSFLIB,
                   TYPE-@FILE,
                   PARA-YMNOMEIF,
                   UNIT-1
         IF        @PGMEC    ^=   0    THEN
                   ?KEKA4    :=   '納品明細データＰＣ転送処理'
                   GOTO ABEND
         ELSE
                   GOTO RTN
         END

/.##ジュンテンドー納品明細書発行データ作成##./
OUTPUT08:

    ?STEP :=  'OUTPUT08'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    ?PGNAME   := %NAME(?PGCD )
    ?LIBNAME  := %NAME(?LIBCD)
    ?PGC      := %NCAT(?PGNAME,?LIBNAME)
    ?PGN      := %STRING(?PGC)
    ?MSGX :=  'SORTF-IN   '
    SNDMSG    ?MSGX,TO-XCTL
    ?DENPYON  :=    %STRING(?DENPYO)
    ?MSGX     :=    '## 伝票ﾌｧｲﾙ名 = ' && ?DENPYON && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    /. データＳＯＲＴ ./
    SORTF  INFILE-?DENPYO,
           OUTFILE-?DENPYO,
       /.  RCD-50,  ./
           RCD-10,
           KEY-'F323!A',             /.取引先ＣＤ./
           KEY1-'F304!A',            /.直接納品先ＣＤ./
           KEY2-'F308!A',            /.最終納品先ＣＤ./
           KEY3-'F112!A',            /.作成日時./
           KEY4-'F344!A',            /.発注日./
           KEY5-'F346!A',            /.最終納品先納品日./
           KEY6-'F342!A',            /.商品分類（大）部門ＣＤ./
           KEY7-'F331!A',            /.便_./
           KEY8-'F302!A',            /.取引番号./
           KEY9-'F402!A',            /.行番号./
           RCDL-@DSP
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4    :=   '伝票データＳＯＲＴ処理'
              GOTO ABEND
    END
    ?MSGX :=  'SORTF-OUT  '
    SNDMSG    ?MSGX,TO-XCTL

    /. 処理前データクリア ./
    CLRFILE   FILE-JTNOMEIF.BMSFLIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4    :=   '処理前データクリア処理'
              GOTO ABEND
    END

        /. 納品明細データ作成 ./
        OVRF      FILE-BMSDENF,TOFILE-?DENPYO
        OVRF      FILE-JTNOMEI1,TOFILE-JTNOMEI1.BMSFLIB
        CALL      PGM-?PGC
        IF        @PGMEC    ^=   0    THEN
                  ?KEKA4    :=   '納品明細データ作成処理'
                  GOTO ABEND
        END

        /. 納品明細データ複写 ./
        CNVFILE   FILE-JTNOMEIF.BMSFLIB,
                  TOFILE-JTNOMESF.BMSFLIB,
                  ADD-@NO,BF-1
         IF        @PGMEC    ^=   0    THEN
                   ?KEKA4    :=   '納品明細データ複写処理'
                   GOTO ABEND
         END

         /. 納品明細データＰＣ転送 ./
         FIMPORT   FILE-JTNOMESF.BMSFLIB,
                   TYPE-@FILE,
                   PARA-JTNOMEIF,
                   UNIT-1
         IF        @PGMEC    ^=   0    THEN
                   ?KEKA4    :=   '納品明細データＰＣ転送処理'
                   GOTO ABEND
         ELSE
                   GOTO RTN
         END

/.##ジョイフルエーケー納品明細書発行データ作成##./
OUTPUT11:

    ?STEP :=  'OUTPUT11'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    ?PGNAME   := %NAME(?PGCD )
    ?LIBNAME  := %NAME(?LIBCD)
    ?PGC      := %NCAT(?PGNAME,?LIBNAME)
    ?PGN      := %STRING(?PGC)
    ?MSGX :=  'SORTF-IN   '
    SNDMSG    ?MSGX,TO-XCTL
    ?DENPYON  :=    %STRING(?DENPYO)
    ?MSGX     :=    '## 伝票ﾌｧｲﾙ名 = ' && ?DENPYON && ' ##'
    SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
    /. データＳＯＲＴ ./
    SORTF  INFILE-?DENPYO,
           OUTFILE-?DENPYO,
           RCD-50,
           KEY-'F323!A',             /.取引先ＣＤ./
           KEY1-'F304!A',            /.直接納品先ＣＤ./
           KEY2-'F344!A',            /.発注日./
           KEY3-'F345!A',            /.直接納品先納品日./
           KEY4-'F302!A',            /.伝票番号./
           KEY5-'F402!A',            /.行番号./
           RCDL-@DSP
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4    :=   '伝票データＳＯＲＴ処理'
              GOTO ABEND
    END
    ?MSGX :=  'SORTF-OUT  '
    SNDMSG    ?MSGX,TO-XCTL

    /. 処理前データクリア ./
    CLRFILE   FILE-JENOMEIF.BMSFLIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4    :=   '処理前データクリア処理'
              GOTO ABEND
    END

        /. 納品明細データ作成 ./
        OVRF      FILE-BMSDENF,TOFILE-?DENPYO
        OVRF      FILE-JENOMEI1,TOFILE-JENOMEI1.BMSFLIB
        CALL      PGM-?PGC
        IF        @PGMEC    ^=   0    THEN
                  ?KEKA4    :=   '納品明細データ作成処理'
                  GOTO ABEND
        END

        /. 納品明細データ複写 ./
        CNVFILE   FILE-JENOMEIF.BMSFLIB,
                  TOFILE-JENOMESF.BMSFLIB,
                  ADD-@NO,BF-1
         IF        @PGMEC    ^=   0    THEN
                   ?KEKA4    :=   '納品明細データ複写処理'
                   GOTO ABEND
         END

         /. 納品明細データＰＣ転送 ./
         FIMPORT   FILE-JENOMESF.BMSFLIB,
                   TYPE-@FILE,
                   PARA-JENOMEIF,
                   UNIT-1
         IF        @PGMEC    ^=   0    THEN
                   ?KEKA4    :=   '納品明細データＰＣ転送処理'
                   GOTO ABEND
         ELSE
                   GOTO RTN
         END

/.##ビバホーム納品明細書発行データ作成##./
OUTPUT10:

    ?STEP :=  'OUTPUT10'
    ?MSGX :=  '***   '  && ?STEP   &&   '        ***'
    SNDMSG    ?MSGX,TO-XCTL
    ?PGNAME   := %NAME(?PGCD )
    ?LIBNAME  := %NAME(?LIBCD)
    ?PGC      := %NCAT(?PGNAME,?LIBNAME)
    ?PGN      := %STRING(?PGC)
  /.?MSGX :=  'SORTF-IN   '                                ./
  /.SNDMSG    ?MSGX,TO-XCTL                                ./
  /.?DENPYON  :=    %STRING(?DENPYO)                       ./
  /.?MSGX     :=    '## 伝票ﾌｧｲﾙ名 = ' && ?DENPYON && ' ##'./
  /.SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES             ./
    /. データＳＯＲＴ ./
  /.SORTF  INFILE-?DENPYO,   ./
  /.       OUTFILE-?DENPYO,  ./
  /.       RCD-50,           ./
  /.       KEY-'F323!A',     ./      /.取引先ＣＤ./
  /.       KEY1-'F304!A',    ./      /.直接納品先ＣＤ./
  /.       KEY2-'F344!A',    ./      /.発注日./
  /.       KEY3-'F345!A',    ./      /.直接納品先納品日./
  /.       KEY4-'F302!A',    ./      /.伝票番号./
  /.       KEY5-'F402!A',    ./      /.行番号./
  /.       RCDL-@DSP         ./
  /.IF        @PGMEC    ^=   0    THEN                  ./
  /.          ?KEKA4    :=   '伝票データＳＯＲＴ処理'   ./
  /.          GOTO ABEND                                ./
  /.END                                                 ./
  /.?MSGX :=  'SORTF-OUT  '                             ./
  /.SNDMSG    ?MSGX,TO-XCTL                             ./

    /. 処理前データクリア ./
    CLRFILE   FILE-VHNOMEIF.BMSFLIB
    IF        @PGMEC    ^=   0    THEN
              ?KEKA4    :=   '処理前データクリア処理'
              GOTO ABEND
    END

        /. 納品明細データ作成 ./
        OVRF      FILE-WKDENVH1,TOFILE-WKDENVH1.BMSFLIB
        OVRF      FILE-VHNOMEI1,TOFILE-VHNOMEI1.BMSFLIB
        CALL      PGM-?PGC
        IF        @PGMEC    ^=   0    THEN
                  ?KEKA4    :=   '納品明細データ作成処理'
                  GOTO ABEND
        END

        /. 納品明細データ複写 ./
        CNVFILE   FILE-VHNOMEIF.BMSFLIB,
                  TOFILE-VHNOMESF.BMSFLIB,
                  ADD-@NO,BF-1
        IF        @PGMEC    ^=   0    THEN
                  ?KEKA4    :=   '納品明細データ複写処理'
                 GOTO ABEND
        END

        ?MSGX :=  'SORTF-IN   '
        SNDMSG    ?MSGX,TO-XCTL
        ?DENPYON  :=    'VHNOMESF'
        ?MSGX     :=    '## 伝票ﾌｧｲﾙ名 = ' && ?DENPYON && ' ##'
        SNDMSG MSG-?MSGX,TO-XCTL.@ORGPROF,JLOG-@YES
        /. データＳＯＲＴ ./
           SORTF  INFILE-VHNOMESF.BMSFLIB,
                  OUTFILE-VHNOMESF.BMSFLIB,
                  RCD-547,
                  KEY-'F01!A',           /.受信日./
                  KEY1-'F02!A',          /.受信時刻./
                  KEY2-'F03!A',          /.受信取引先./
                  KEY3-'F04!A',          /.倉庫ＣＤ./
                  KEY4-'F05!A',          /.頁./
                  KEY5-'F07!A',          /.ＳＥＱ./
                  RCDL-@DSP
           IF     @PGMEC    ^=   0    THEN
                  ?KEKA4    :=   '伝票データＳＯＲＴ処理'
                  GOTO ABEND
           END
           ?MSGX :=  'SORTF-OUT  '
           SNDMSG    ?MSGX,TO-XCTL

         /. 納品明細データＰＣ転送 ./
         FIMPORT   FILE-VHNOMESF.BMSFLIB,
                   TYPE-@FILE,
                   PARA-VHNOMEIF,
                   UNIT-1
         IF        @PGMEC    ^=   0    THEN
                   ?KEKA4    :=   '納品明細データＰＣ転送処理'
                   GOTO ABEND
         ELSE
                   GOTO RTN
         END

RTN:

    ?MSGX :=  '***   '  && ?PGMID  &&   ' END    ***'
    SNDMSG    ?MSGX,TO-XCTL

    RETURN    PGMEC-@PGMEC

ASSERR:
    ?OPR1  :=  '＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃'
    ?OPR2  :=  '＃　他の倉庫で伝票発行中です。　　　　　　　　＃'
    ?OPR3  :=  '＃　しばらくお待ち頂いてから実行して下さい。　＃'
    ?OPR4  :=  '＃　　　　　　　　　　　　　　　　　　　　　　＃'
    ?OPR5  :=  '＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃＃'
    OVRDSPF  FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    CALL     OHOM0900.TOKELIB,PARA-
                            (?OPR1,?OPR2,?OPR3,?OPR4,?OPR5)

    GOTO PSETPF

ABEND:  /.ﾌﾟﾛｸﾞﾗﾑ異常終了時処理./

    ?PGMEC    :=    @PGMEC
    ?PGMEM    :=    @PGMEM
    ?PGMECX   :=    %STRING(?PGMEC)

    OVRDSPF   FILE-DSPF,TOFILE-DSPF.XUCL,MEDLIB-TOKELIB
    ?KEKA1 :=  '処理が異常終了しました。'
    ?KEKA2 :=  'ログリストを採取後、ＮＡＶへ連絡'
    ?KEKA3 :=  ''
    CALL SMG0030I.TOKELIB
                    ,PARA-('2',?PGNM,?KEKA1,?KEKA2,?KEKA3,?KEKA4)

    ?MSG(1)   :=   '### ' && ?PGMID && ' ABEND' &&   '    ###'
    ?MSG(2)   :=   '###' && ' PGMEC = ' &&
                    %SBSTR(?PGMECX,8,4) &&         '      ###'
    ?MSG(3)   :=   '###' && ' STEP = '  && ?STEP
                                                   && '   ###'
    FOR ?I    :=     1 TO 3
        DO     ?MSGX :=   ?MSG(?I)
               SNDMSG    ?MSGX,TO-XCTL
    END

    RETURN    PGMEC-?PGMEC

```
