****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　流通ＢＭＳオンライン　　　　　　　*
*    業務名　　　　　　　：　流通ＢＭＳデータ変換　　　　　　　*
*    モジュール名　　　　：　サカタ売上フォーマット変換処理　　*
*    　　　　　　　　　　　　　　　　（ハンズマン）　　　　　　*
*    作成日　　　　　　　：　2023/11/10                        *
*    作成者　　　　　　　：　NAV TAKAHASHI                     *
*    処理概要　　　　　　：　流通ＢＭＳ発注ＭＳＧを読み、サカ　*
*                            タ売上伝票フォーマットに変換する。*
*    　　　　　　　　　　：　サブ商品変換ＴＢＬ参照VER.        *
*    　　　　　　　　　　：　                                  *
*    更新日　　　　　　　：　                                  *
*    更新概要　　　　　　：　　　　　　　　　　　　　　　　　　*
*    　　　　　　　　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SSY4521B.
*                 流用：SSY4466B ジュンテンドー
 AUTHOR.                NAV.
 DATE-WRITTEN.          2021/10/06.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*流通ＢＭＳ発注メッセージ
     SELECT   BMSHACF   ASSIGN    TO        DA-01-VI-BMSHACL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       HAC-F011  HAC-F012
                                            HAC-F013  HAC-F02
                                            HAC-F308  HAC-F346
                                            HAC-F302  HAC-F402
                                            WITH DUPLICATES
                        FILE  STATUS   IS   HAC-STATUS.
*変換伝票データ
     SELECT   JHSHENL1  ASSIGN    TO        DA-01-VI-JHSHENL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       HEN-F46
                                            HEN-F47   HEN-F01
                                            HEN-F02   HEN-F03
                        FILE  STATUS   IS   HEN-STATUS.
*取引先マスタ
     SELECT   TOKMS2    ASSIGN    TO        DA-01-VI-TOKMS2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TOK-F01
                        FILE  STATUS   IS   TOK-STATUS.
*
*店舗マスタ
*    SELECT   TENMS1    ASSIGN    TO        DA-01-VI-TENMS1
*                       ORGANIZATION        INDEXED
*                       ACCESS    MODE      RANDOM
*                       RECORD    KEY       TEN-F52
*                                           TEN-F011
*                       FILE  STATUS   IS   TEN-STATUS.
*
*商品変換テーブル
     SELECT   SHOTBL1   ASSIGN    TO        DA-01-VI-SHOTBL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TBL-F01   TBL-F02
                        FILE STATUS    IS   TBL-STATUS.
*SUB商品変換テーブル
*    SELECT   SUBTBLL1  ASSIGN    TO        DA-01-VI-SUBTBLL1
*                       ORGANIZATION        INDEXED
*                       ACCESS    MODE      RANDOM
*                       RECORD    KEY       SUB-F01   SUB-F02
*                       FILE STATUS    IS   SUB-STATUS.
*商品名称マスタ
     SELECT   MEIMS1    ASSIGN    TO        DA-01-VI-MEIMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       MEI-F011
                                            MEI-F012
                        FILE STATUS    IS   MEI-STATUS.
*ルート条件マスタ
     SELECT   JHMRUTL1  ASSIGN    TO        DA-01-VI-JHMRUTL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       RUT-F01   RUT-F02
                                            RUT-F03
                        FILE STATUS    IS   RUT-STATUS.
*出荷場所件数マスタ
     SELECT   JSMKENL1  ASSIGN    TO        DA-01-VI-JSMKENL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       KEN-F01   KEN-F02
                                            KEN-F03   KEN-F04
                        FILE  STATUS   IS   KEN-STATUS.
*当日スケジュールマスタ
     SELECT   JSMDAYL1  ASSIGN    TO        DA-01-VI-JSMDAYL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TJS-F01  TJS-F02
                                            TJS-F03
                        FILE  STATUS   IS   TJS-STATUS.
*ＶＬＤ５００
     SELECT   VLD500    ASSIGN    TO        VLD500
                        FILE  STATUS   IS   VLD-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    流通ＢＭＳ発注メッセージ
******************************************************************
 FD  BMSHACF
                        LABEL RECORD   IS   STANDARD.
     COPY     BMSHACF   OF        XFDLIB
              JOINING   HAC  AS   PREFIX.
******************************************************************
*    取引先マスタ
******************************************************************
 FD  TOKMS2             LABEL RECORD   IS   STANDARD.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
*
******************************************************************
*    店舗マスタ
******************************************************************
*FD  TENMS1             LABEL RECORD   IS   STANDARD.
*    COPY     HTENMS    OF        XFDLIB
*             JOINING   TEN       PREFIX.
*
******************************************************************
*    商品変換テーブル
******************************************************************
 FD  SHOTBL1            LABEL RECORD   IS   STANDARD.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   TBL       PREFIX.
******************************************************************
*    SUB商品変換テーブル
******************************************************************
*FD  SUBTBLL1           LABEL RECORD   IS   STANDARD.
*    COPY     SUBTBLL1  OF        XFDLIB
*             JOINING   SUB       PREFIX.
******************************************************************
*    商品名称マスタ
******************************************************************
 FD  MEIMS1             LABEL RECORD   IS   STANDARD.
     COPY     HMEIMS    OF        XFDLIB
              JOINING   MEI       PREFIX.
******************************************************************
*    ルート条件マスタ
******************************************************************
 FD  JHMRUTL1           LABEL RECORD   IS   STANDARD.
     COPY     JHMRUTF   OF        XFDLIB
              JOINING   RUT       PREFIX.
******************************************************************
*    出荷場所別件数ファイル
******************************************************************
 FD  JSMKENL1           LABEL RECORD   IS   STANDARD.
     COPY     JSMKENF   OF        XFDLIB
              JOINING   KEN       PREFIX.
******************************************************************
*    当日スケジュールマスタ
******************************************************************
 FD  JSMDAYL1           LABEL RECORD   IS   STANDARD.
     COPY     JSMDAYF   OF        XFDLIB
              JOINING   TJS       PREFIX.
******************************************************************
*    変換伝票データ　ＲＬ＝１０２０
******************************************************************
 FD  JHSHENL1
                        LABEL RECORD   IS   STANDARD.
*2013/03/01 NAV ST 項目名が足りない為、ＦＧＤ名変更
*****COPY     JHSHENF   OF        XFDLIB
     COPY     SHTDENF   OF        XFDLIB
*2013/03/01 NAV ED
              JOINING   HEN  AS   PREFIX.
*
******************************************************************
*    ＶＬＤ５００
******************************************************************
 FD  VLD500.
 01  VLD-REC.
     03  VLD-F01           PIC  X(02).
     03  VLD-F02           PIC  9(03).
     03  VLD-F03           PIC  X(02).
     03  VLD-F04           PIC  X(08).
     03  VLD-F05           PIC  9(06).
     03  VLD-F06           PIC  9(01).
     03  VLD-F07           PIC  X(02).
     03  VLD-F08           PIC  9(02).
     03  VLD-F09           PIC  9(02).
     03  VLD-F10           PIC  9(04).
     03  VLD-F11           PIC  9(08).
     03  VLD-F12           PIC  9(04).
     03  VLD-F13           PIC  9(08).
     03  FILLER            PIC  X(48).
*
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*ワークに発注メッセージを展開
     COPY   BMSHACF  OF XFDLIB  JOINING   HWK  AS   PREFIX.
*    ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  IDX                     PIC  9(02)     VALUE  ZERO.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT                 PIC  9(08)     VALUE  ZERO.
 01  CNT-KENSU               PIC  9(08)     VALUE  ZERO.
 01  CNT-KENSU-D             PIC  9(08)     VALUE  ZERO.
 01  CNT-MAISU               PIC  9(08)     VALUE  ZERO.
 01  CNT-KENSU2              PIC  9(08)     VALUE  ZERO.
 01  CNT-KENSU-D2            PIC  9(08)     VALUE  ZERO.
 01  CNT-MAISU2              PIC  9(08)     VALUE  ZERO.
 01  CNT-GYO                 PIC  9(02)     VALUE  ZERO.
 01  INV-RUT                 PIC  9(01)     VALUE  ZERO.
 01  FLG-TOK                 PIC  9(01)     VALUE  ZERO.
 01  HTOKMS-INV-FLG          PIC  X(03)     VALUE  SPACE.
 01  TENMS1-INV-FLG          PIC  X(03)     VALUE  SPACE.
 01  JHMRUTF-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  FURIWAKE-CD             PIC  9(01)     VALUE  ZERO.
 01  DAIHYO-BASYO-CD         PIC  X(02)     VALUE  SPACE.
 01  SYUKA-BASYO             PIC  X(02)     VALUE  SPACE.
*ブレイク退避領域
 01  WK-KEY.
     03  WK-HAC-F308         PIC  X(13).
     03  WK-HAC-F346         PIC  9(08).
     03  WK-HAC-F302         PIC  X(10).
     03  WK-HAC-F402         PIC  X(04).
*出荷場所退避
 01  WK-BASYO.
     03  WK-SYUKA-BASYO      PIC  X(02)     VALUE  SPACE.
*
 01  WK-HAC-F435-X.
     03  WK-HAC-F435       PIC  N(30).
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
 01  WK-ST.
     03  HAC-STATUS        PIC  X(02).
     03  HEN-STATUS        PIC  X(02).
     03  TOK-STATUS        PIC  X(02).
*
*    03  TEN-STATUS        PIC  X(02).
*
     03  TBL-STATUS        PIC  X(02).
     03  SUB-STATUS        PIC  X(02).
     03  MEI-STATUS        PIC  X(02).
     03  RUT-STATUS        PIC  X(02).
     03  KEN-STATUS        PIC  X(02).
     03  TJS-STATUS        PIC  X(02).
     03  VLD-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY4521B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY4521B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY4521B".
         05  FILLER         PIC   X(11)  VALUE
                                         " ABEND *** ".
     03  ABEND-FILE.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  AB-FILE        PIC   X(08).
         05  FILLER         PIC   X(06)  VALUE " ST = ".
         05  AB-STS         PIC   X(02).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  SEC-NAME.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(07)  VALUE " SEC = ".
         05  S-NAME         PIC   X(30).
     03  MSG-IN.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " INPUT = ".
         05  IN-CNT         PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  MSG-OUT.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " OUTPUT= ".
         05  OUT-CNT        PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
*
*    ルート変換
 01  WK-RUTO.
     03  WK-RUTO-S          PIC       X(02).
     03  WK-RUTO-S-R        REDEFINES WK-RUTO-S.
         05  WK-RUTO-H      PIC       9(02).
*伝票番号
 01  WK-DEN-NO.
     03  WK-DEN-S          PIC        X(06).
     03  WK-DEN-S-R        REDEFINES WK-DEN-S.
         05  WK-DEN-H      PIC        9(06).
*行番号
 01  WK-GYO-NO.
     03  WK-GYO-S          PIC        X(01).
     03  WK-GYO-S-R        REDEFINES WK-GYO-S.
         05  WK-GYO-H      PIC        9(01).
*店舗ＣＤ
 01  WK-TEN-NO.
     03  WK-TEN-S          PIC        X(02).
     03  WK-TEN-S-R        REDEFINES WK-TEN-S.
         05  WK-TEN-H      PIC        9(02).
*
 01  WK-CEN-NO.
     03  WK-CEN-S          PIC        X(05).
     03  WK-CEN-S-R        REDEFINES WK-CEN-S.
         05  WK-CEN-H      PIC        9(05).
*
 01  WK-CET-NO.
     03  WK-CET-S          PIC        X(03).
     03  WK-CET-S-R        REDEFINES WK-CET-S.
         05  WK-CET-H      PIC        9(03).
*バッチ時刻編集
 01  WK-BATTI-JIKAN.
     03  WK-JIKAN          PIC        9(02)  VALUE  ZERO.
     03  WK-JIKOKU         PIC        9(02)  VALUE  ZERO.
 01  WK-BATTI-JIKAN-2      PIC        9(04)  VALUE  ZERO.
*
 01  BATTI-JIKAN           PIC        9(08)  VALUE  ZERO.
 01  BATTI-JIKOKU          PIC        9(04)  VALUE  ZERO.
 01  BATTI-TOKCD           PIC        9(08)  VALUE  ZERO.
 01  BATTI-JIKOKU-FLG      PIC        X(01)  VALUE  SPACE.
 01  BATTI-CHK             PIC        X(01)  VALUE  SPACE.
*
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 LINKAGE                SECTION.
 01  PARA-AREA.
     03  PARA-JDATE         PIC   9(08).
     03  PARA-JTIME         PIC   9(04).
     03  PARA-JTORICD       PIC   9(08).
     03  PARA-KSYU          PIC   X(01).
     03  PARA-YUSEN         PIC   X(01).
*01  PARA-JIKAN2            PIC   9(04).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-AREA.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   BMSHACF.
     MOVE      "BMSHACL1"   TO   AB-FILE.
     MOVE      HAC-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JHSHENL1.
     MOVE      "JHSHENL1"   TO   AB-FILE.
     MOVE      HEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC3           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   TOKMS2.
     MOVE      "TOKMS2"     TO   AB-FILE.
     MOVE      TOK-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC4           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   SHOTBL1.
     MOVE      "SHOTBL1"    TO   AB-FILE.
     MOVE      TBL-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC5           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   MEIMS1.
     MOVE      "MEIMS1"     TO   AB-FILE.
     MOVE      MEI-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC6           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JSMKENL1.
     MOVE      "JSMKENL1"   TO   AB-FILE.
     MOVE      KEN-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC7           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JSMDAYL1.
     MOVE      "JSMDAYL1"   TO   AB-FILE.
     MOVE      TJS-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC8           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JHMRUTL1.
     MOVE      "JHMRUTL1"   TO   AB-FILE.
     MOVE      RUT-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC9           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   VLD500.
     MOVE      "VLD500  "   TO   AB-FILE.
     MOVE      VLD-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
*FILEERR-SEC10          SECTION.
*    USE       AFTER    EXCEPTION
*                       PROCEDURE   SUBTBLL1.
*    MOVE      "SUBTBLL1"   TO   AB-FILE.
*    MOVE      SUB-STATUS   TO   AB-STS.
*    DISPLAY   MSG-ABEND         UPON CONS.
*    DISPLAY   SEC-NAME          UPON CONS.
*    DISPLAY   ABEND-FILE        UPON CONS.
*    MOVE      4000         TO   PROGRAM-STATUS.
*    STOP      RUN.
*
*
*FILEERR-SEC11          SECTION.
*    USE       AFTER    EXCEPTION
*                       PROCEDURE   TENMS1.
*    MOVE      "TENMS1"     TO   AB-FILE.
*    MOVE      TEN-STATUS   TO   AB-STS.
*    DISPLAY   MSG-ABEND         UPON CONS.
*    DISPLAY   SEC-NAME          UPON CONS.
*    DISPLAY   ABEND-FILE        UPON CONS.
*    MOVE      4000         TO   PROGRAM-STATUS.
*    STOP      RUN.
*
 END     DECLARATIVES.
*****************************************************************
*                                                                *
******************************************************************
 GENERAL-PROCESS       SECTION.
*
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC
              UNTIL     END-FLG    =    "END".
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     I-O       BMSHACF.
     OPEN     INPUT     SHOTBL1   MEIMS1
                        JHMRUTL1  TOKMS2.
     OPEN     EXTEND    JHSHENL1.
     OPEN     I-O       JSMKENL1  JSMDAYL1.
     OPEN     OUTPUT    VLD500.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     SPACE     TO        END-FLG.
     MOVE     ZERO      TO        RD-CNT    WRT-CNT.
     MOVE     ZERO      TO        IN-CNT    OUT-CNT.
*
******************
*システム日付編集*
******************
     ACCEPT      SYS-DATE  FROM      DATE.
     MOVE       "3"        TO        LINK-IN-KBN.
     MOVE        SYS-DATE  TO        LINK-IN-YMD6.
     CALL       "SKYDTCKB"   USING   LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD8.
     IF          LINK-OUT-RET   =    ZERO
         MOVE    LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
         MOVE    ZERO           TO   SYS-DATEW
     END-IF.
*バッチ時刻＋１分算出
     MOVE        ZERO           TO   WK-BATTI-JIKAN
                                     WK-BATTI-JIKAN-2.
     MOVE        PARA-JTIME     TO   WK-BATTI-JIKAN.
     ADD         1              TO   WK-JIKOKU.
     IF   WK-JIKOKU  =  60
          ADD    1              TO   WK-JIKAN
          MOVE   ZERO           TO   WK-JIKOKU
          MOVE   WK-BATTI-JIKAN TO   WK-BATTI-JIKAN-2
     ELSE
          MOVE   WK-BATTI-JIKAN TO   WK-BATTI-JIKAN-2
     END-IF.
*    DISPLAY NC"＃バッチ時間２" " = " WK-JIKAN ":" WK-JIKOKU
*            UPON CONS.
*    DISPLAY NC"＃バッチ２" " = " WK-BATTI-JIKAN-2 UPON CONS.
*取引先マスタ索引
*****DISPLAY "## JTORICD = " PARA-JTORICD UPON CONS.
     PERFORM  HTOKMS-READ-SEC.
     IF  HTOKMS-INV-FLG  = "INV"
         MOVE  4000          TO   PROGRAM-STATUS
         DISPLAY NC"＃取引先マスタ無　異常！！＃" UPON CONS
         STOP  RUN
     ELSE
*********振分倉庫ＣＤ／代表場所ＣＤ
         MOVE  TOK-F95       TO   FURIWAKE-CD
         MOVE  TOK-F81       TO   DAIHYO-BASYO-CD
     END-IF.
*
*流通ＢＭＳ発注メッセージスタート
     MOVE     SPACE          TO   HAC-REC.
     INITIALIZE                   HAC-REC.
     MOVE     PARA-JDATE     TO   HAC-F011.
     MOVE     PARA-JTIME     TO   HAC-F012.
     MOVE     PARA-JTORICD   TO   HAC-F013.
     START    BMSHACF  KEY  >=    HAC-F011  HAC-F012  HAC-F013
                                  HAC-F02   HAC-F308  HAC-F346
                                  HAC-F302  HAC-F402
         INVALID   KEY
         DISPLAY NC"＃対象の取引先がありません！！１"
                 UPON CONS
         DISPLAY "# ｼﾞｭｼﾝﾋﾞ   = " PARA-JDATE " #" UPON CONS
         DISPLAY "# ｼﾞｭｼﾝｼﾞｶﾝ = " PARA-JTIME "     #" UPON CONS
         DISPLAY "# ﾄﾘﾋｷｻｷCD  = " PARA-JTORICD " #" UPON CONS
         MOVE "END"     TO   END-FLG
         GO             TO   INIT-EXIT
     END-START.
*流通ＢＭＳ発注メッセージ読込
     PERFORM  BMSHACF-READ-SEC.
     IF       END-FLG = "END"
              DISPLAY NC"＃対象の取引先がありません！！２"
                      UPON CONS
              DISPLAY "# ｼﾞｭｼﾝﾋﾞ   = " PARA-JDATE " #" UPON CONS
              DISPLAY "# ｼﾞｭｼﾝｼﾞｶﾝ = " PARA-JTIME " #" UPON CONS
              DISPLAY "# ﾄﾘﾋｷｻｷCD  = " PARA-JTORICD " #" UPON CONS
     ELSE
**************店舗ＣＤ、納品日、伝票番号、行番号を退避する
              INITIALIZE                WK-KEY
              MOVE  HAC-F308       TO   WK-HAC-F308
              MOVE  HAC-F346       TO   WK-HAC-F346
              MOVE  HAC-F302       TO   WK-HAC-F302
              MOVE  HAC-F402       TO   WK-HAC-F402
**************トータル伝票枚数カウント
              MOVE  1              TO   CNT-MAISU
*             IF    HAC-F330  =  "01"
*                   MOVE  1              TO   CNT-MAISU
*             ELSE
*                   MOVE  1              TO   CNT-MAISU2
*             END-IF
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　流通ＢＭＳ発注メッセージ読込
****************************************************************
 BMSHACF-READ-SEC          SECTION.
*
     READ     BMSHACF
              AT  END       MOVE  "END"   TO  END-FLG
                            GO TO BMSHACF-READ-EXIT
     END-READ.
*指定バッチ_チェック
     IF     ( PARA-JDATE    =  HAC-F011 ) AND
            ( PARA-JTIME    =  HAC-F012 ) AND
            ( PARA-JTORICD  =  HAC-F013 )
              CONTINUE
     ELSE
              MOVE      "END"     TO   END-FLG
              GO                  TO   BMSHACF-READ-EXIT
     END-IF.
*既に更新済みは対象としない。
     IF       HAC-FIL7(100:1)  =  "1"
              GO                  TO   BMSHACF-READ-SEC
     END-IF.
*倉庫ＣＤが既にセットされていたら終了
     IF       HAC-F02   NOT =  SPACE
              MOVE      "END"     TO   END-FLG
              GO                  TO   BMSHACF-READ-EXIT
     END-IF.
*
     ADD      1         TO        RD-CNT.
 BMSHACF-READ-EXIT.
     EXIT.
****************************************************************
*　　取引先マスタ索引
****************************************************************
 HTOKMS-READ-SEC           SECTION.
*
     MOVE    SPACE         TO        TOK-REC.
     INITIALIZE                      TOK-REC.
     MOVE    PARA-JTORICD  TO        TOK-F01.
     READ    TOKMS2
             INVALID
             MOVE  "INV"   TO        HTOKMS-INV-FLG
             NOT  INVALID
             MOVE  SPACE   TO        HTOKMS-INV-FLG
     END-READ.
*
 HTOKMS-READ-EXIT.
     EXIT.
****************************************************************
*　　店舗マスタ索引
****************************************************************
*TENMS1-READ-SEC           SECTION.
*
*    READ    TENMS1
*            INVALID
*            MOVE  "INV"   TO        TENMS1-INV-FLG
*            NOT  INVALID
*            MOVE  SPACE   TO        TENMS1-INV-FLG
*    END-READ.
*
*TENMS1-READ-EXIT.
*    EXIT.
****************************************************************
*　　ルート条件マスタ索引
****************************************************************
 JHMRUTF-READ-SEC          SECTION.
*
     MOVE     HEN-F01      TO        RUT-F01.
     MOVE     SPACE        TO        RUT-F02.
     MOVE     HAC-F328     TO        RUT-F03.
*
     READ     JHMRUTL1
         INVALID
           MOVE  "INV"     TO        JHMRUTF-INV-FLG
         NOT INVALID
           MOVE  SPACE     TO        JHMRUTF-INV-FLG
     END-READ.
*
 JHMRUTF-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"          TO   S-NAME.
*ブレイクチェック（店舗、納品日、伝票番号、行番号）
**   DISPLAY "F308 = "  HAC-F308 "-" WK-HAC-F308 UPON CONS.
**   DISPLAY "F346 = "  HAC-F346 "-" WK-HAC-F346 UPON CONS.
**   DISPLAY "F302 = "  HAC-F302 "-" WK-HAC-F302 UPON CONS.
**   DISPLAY "F402 = "  HAC-F402 "-" WK-HAC-F402 UPON CONS.
     IF       HAC-F308  NOT =  WK-HAC-F308
     OR       HAC-F346  NOT =  WK-HAC-F346
     OR       HAC-F302  NOT =  WK-HAC-F302
*             出荷場所件数マスタ出力
              PERFORM  JSMKENL1-WRT-SEC
*             トータル伝票枚数カウント
              IF  BATTI-CHK  =  SPACE
                  ADD   1         TO   CNT-MAISU
              ELSE
                  ADD   1         TO   CNT-MAISU2
              END-IF
*             伝票明細数カウンター初期化
              MOVE  ZERO      TO   CNT-KENSU-D CNT-KENSU-D2
*             店舗ＣＤ、納品日、伝票番号、行番号を退避する
              INITIALIZE                WK-KEY
              MOVE  HAC-F308  TO   WK-HAC-F308
              MOVE  HAC-F346  TO   WK-HAC-F346
              MOVE  HAC-F302  TO   WK-HAC-F302
              MOVE  HAC-F402  TO   WK-HAC-F402
     END-IF.
*
*伝票データを出力する。
     PERFORM  TENSO-SEC.
*トータル明細数、伝票明細数カウント
 MAIN010.
     IF  BATTI-CHK  =  SPACE
         ADD  1               TO   CNT-KENSU CNT-KENSU-D
     ELSE
         ADD  1               TO   CNT-KENSU2 CNT-KENSU-D2
     END-IF.
*
 MAIN020.
*発注メッセージの出荷場所を更新
     MOVE      SPACE          TO   HWK-REC.
     INITIALIZE                    HWK-REC.
     MOVE      HAC-REC        TO   HWK-REC.
     MOVE  WK-SYUKA-BASYO     TO   HWK-F02  HAC-F02.
**   DISPLAY "WK-SYUKA-BASYO = " WK-SYUKA-BASYO  UPON CONS.
     MOVE     BATTI-JIKOKU    TO   HWK-F012.
     MOVE     "1"             TO   HWK-FIL7(100:1).
*
 MAIN030.
*****DELETE   BMSHACF.
*
 MAIN040.
     MOVE     SPACE           TO   HAC-REC.
     INITIALIZE                    HAC-REC.
     MOVE     HWK-REC         TO   HAC-REC.
 MAIN050.
*****WRITE  HAC-REC.
     REWRITE  HAC-REC.
*
*流通ＢＭＳ発注メッセージ読込
     PERFORM  BMSHACF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　変換伝票転送                                    *
****************************************************************
 TENSO-SEC             SECTION.
*
     MOVE    "TENSO-SEC"   TO        S-NAME.
*レコード初期化
     MOVE    SPACE         TO        HEN-REC.
     INITIALIZE                      HEN-REC.
*取引先コード
     MOVE  HAC-F013        TO        HEN-F01.
*伝票番号
     MOVE  HAC-F302(1:6)   TO        WK-DEN-S.
     MOVE  WK-DEN-H        TO        HEN-F02.
     MOVE  HEN-F02         TO        HEN-F23.
*行番号
     MOVE  HAC-F402(1:1)   TO        WK-GYO-S.
     MOVE  WK-GYO-S        TO        HEN-F03.
*取区
     MOVE  40              TO        HEN-F051.
     MOVE  NC"売上伝票"    TO        HEN-F052.
*担当者コード
     MOVE  99              TO        HEN-F06.
*店コード
     MOVE  HAC-F308(1:2)   TO        WK-TEN-S.
     MOVE  WK-TEN-H        TO        HEN-F07.
*#LINKS連携用に直接納品先ＣＤ、最終納品先ＣＤをセットする。
**** MOVE  HAC-F304(1:4)   TO        HEN-XI0004(1:6).
*    MOVE  HAC-F308(1:4)   TO        HEN-XI0004(7:6).
*    MOVE  HAC-F331        TO        HEN-XI0004(13:2).
*    MOVE  HAC-F330        TO        HEN-XI0004(15:2).
**** MOVE  HAC-F304(2:3)   TO        HEN-XI0004(17:3).
     MOVE  ZERO            TO        HEN-F40.
     MOVE  PARA-JTIME      TO        BATTI-JIKOKU.
*#2019/06/27 NAV ST
     MOVE  SPACE           TO        BATTI-CHK.
*納品先がセンター以外　：０をセット
*****IF    HAC-F330  =  "01"
*          MOVE  ZERO      TO        HEN-F40
*          MOVE  PARA-JTIME  TO      BATTI-JIKOKU
*    ELSE
*          MOVE  HAC-F304(1:4)  TO   WK-CEN-S
*****      MOVE  WK-CEN-H       TO   HEN-F40
*?         MOVE  WK-BATTI-JIKAN-2 TO BATTI-JIKOKU
*?         MOVE  "1"            TO   BATTI-JIKOKU-FLG
*?         MOVE  "1"            TO   BATTI-CHK
*****END-IF.
*##########################################################
*出荷場所／伝発場所
*    商品変換テーブル検索
     MOVE  SPACE           TO        TBL-REC.
     INITIALIZE                      TBL-REC.
     MOVE  HEN-F01         TO        TBL-F01.
     MOVE  HAC-F413(1:7)   TO        TBL-F02  HEN-F25.
     READ    SHOTBL1
       INVALID
         MOVE    SPACE     TO        TBL-REC
         INITIALIZE                  TBL-REC
     END-READ.
*    出荷場所／伝発場所
     MOVE  TBL-F04         TO        HEN-F08
                                     HEN-F09.
*2019/05/17↓
*    SUB商品変換テーブル検索
*    MOVE  SPACE           TO        SUB-REC.
*    INITIALIZE                      SUB-REC.
*    MOVE  TBL-F01         TO        SUB-F01.
*    MOVE  TBL-F02         TO        SUB-F02.
*    READ    SUBTBLL1
*      INVALID
*        MOVE    SPACE     TO        SUB-REC
*        INITIALIZE                  SUB-REC
*      NOT INVALID
*        MOVE    SUB-F16   TO        TBL-F04
*    END-READ.
*2019/05/17↑
*
*１行目の出荷場所ＣＤを退避する。
     IF  HEN-F03  =  1
         MOVE  TBL-F04     TO        SYUKA-BASYO
*********#2019/05/17 NAV ST１行目が空白の場合は代表倉庫
         IF    TBL-F04  =  SPACE
               MOVE DAIHYO-BASYO-CD TO   SYUKA-BASYO
         END-IF
*********#2019/05/17 NAV ED
     END-IF.
*発注日
     MOVE  HAC-F344        TO        HEN-F111.
*納品日
     MOVE  HAC-F346        TO        HEN-F112.
*分類（部門）
     MOVE  HAC-F342(1:2)   TO        HEN-F12.
*商品区分　　
     MOVE  SPACE           TO        HEN-F131.
*伝票区分　　
     MOVE  "10"            TO        HEN-F132.
*伝発区分
     MOVE  9               TO        HEN-F134.
*自社商品コード
     MOVE  TBL-F031        TO        HEN-F1411.
*自社商品単品コード
     MOVE  TBL-F032        TO        HEN-F1412.
*商品名　　　
*    商品名称マスタ検索
     MOVE        SPACE     TO        MEI-REC.
     INITIALIZE                      MEI-REC.
     MOVE        TBL-F031  TO        MEI-F011.
     MOVE        TBL-F032  TO        MEI-F012.
     READ    MEIMS1
       INVALID
*        商品名（商品名）
         MOVE  HAC-F417(1:15)     TO   HEN-F1421
         MOVE  HAC-F419(1:15)     TO   HEN-F1422
******** MOVE  HAC-F435           TO   WK-HAC-F435
******** MOVE  WK-HAC-F435-X(1:5) TO   HEN-F1422(11:5)
*        商品名（規格）
*        MOVE  HAC-F419(1:5)    TO   HEN-F1422(11:5)
       NOT INVALID
*        商品名（商品名）
         MOVE  HAC-F417(1:15)     TO   HEN-F1421
         MOVE  HAC-F419(1:15)     TO   HEN-F1422
******** MOVE  HAC-F435           TO   WK-HAC-F435
******** MOVE  WK-HAC-F435-X(1:5) TO   HEN-F1422(11:5)
*        商品名（規格）
*        MOVE  HAC-F419(1:5)    TO   HEN-F1422(11:5)
     END-READ.
*数量
     MOVE  HAC-F453        TO        HEN-F15.
*単
     MOVE  "1"             TO        HEN-F16.
*原価単価
     MOVE  HAC-F446        TO        HEN-F172.
*売価単価
     MOVE  HAC-F448        TO        HEN-F173.
*原価金額
     MOVE  HAC-F445        TO        HEN-F181.
*売価金額
     MOVE  HAC-F447        TO        HEN-F182.
*出荷場所振分方法を特定する。
     EVALUATE   FURIWAKE-CD
*       *ルート条件マスタ
         WHEN   1
         PERFORM  JHMRUTF-READ-SEC
         IF  JHMRUTF-INV-FLG = "INV"
             MOVE  DAIHYO-BASYO-CD  TO  HEN-F48
         ELSE
             MOVE  RUT-F05      TO   HEN-F48
         END-IF
*       *伝票１行目出荷場所
         WHEN   2
         MOVE  SYUKA-BASYO      TO   HEN-F48
*       *代表倉庫
         WHEN   3
         MOVE  DAIHYO-BASYO-CD  TO   HEN-F48
*       *それ以外　代表倉庫
         WHEN   OTHER
         MOVE  DAIHYO-BASYO-CD  TO   HEN-F48
     END-EVALUATE.
*振分倉庫ＣＤを退避する
     MOVE  HEN-F48         TO        WK-SYUKA-BASYO.
*伝票発行区分
     MOVE  9               TO        HEN-F272.
*オンライン区分
     MOVE  1               TO        HEN-F274.
*エントリー区分
     MOVE  1               TO        HEN-F275.
*付番区分
     MOVE  9               TO        HEN-F276.
*量販店区分
     MOVE  "A"             TO        HEN-F278.
*ＷＳ■
     MOVE  1               TO        HEN-F28.
*店舗名（カナ）
     MOVE  HAC-F311        TO        HEN-F30.
*システム日付
     MOVE  SYS-DATEW       TO        HEN-F99.
*受信日付
     MOVE  PARA-JDATE      TO        HEN-F46.
*受信時刻
*****MOVE  PARA-JTIME      TO        HEN-F47.
     MOVE  BATTI-JIKOKU    TO        HEN-F47.
*振分倉庫コード
*タナ番
     MOVE  TBL-F08         TO        HEN-F49.
*訂正前数量
     MOVE  HEN-F15         TO        HEN-F50.
*修正原価単価
     MOVE  HEN-F172        TO        HEN-F512.
*修正売価単価
     MOVE  HEN-F173        TO        HEN-F513.
*修正原価金額
     MOVE  HEN-F181        TO        HEN-F521.
*修正売価金額
     MOVE  HEN-F182        TO        HEN-F522.
*センターコード？（ＦＤＧ項目は「伝票枚数１」）
*納品先がセンターの場合：直接納品先ＣＤをセット
*ルート
*****MOVE     HAC-F328     TO        HEN-F42.
     MOVE     HEN-F48      TO        HEN-F42.
*
*レコード出力
     WRITE  HEN-REC.
*
     ADD    1              TO        WRT-CNT.
*
 TENSO-EXIT.
     EXIT.
****************************************************************
*　　　　　　　出荷場所件数マスタ出力                          *
****************************************************************
 JSMKENL1-WRT-SEC        SECTION.
*
     MOVE   "JSMKENL1-WRT-SEC"  TO   S-NAME.
     MOVE    SPACE         TO        KEN-REC.
     INITIALIZE                      KEN-REC.
     MOVE    PARA-JDATE    TO        KEN-F01.
*****MOVE    PARA-JTIME    TO        KEN-F02.
     MOVE    BATTI-JIKOKU  TO        KEN-F02.
     MOVE    HEN-F01       TO        KEN-F03.
     MOVE    WK-SYUKA-BASYO  TO      KEN-F04.
     READ    JSMKENL1
       INVALID
         CONTINUE
       NOT INVALID
         GO  TO   JSMKENL1-010
     END-READ.
*
     MOVE    SPACE         TO        KEN-REC.
     INITIALIZE                      KEN-REC.
     MOVE    PARA-JDATE    TO        KEN-F01.
*****MOVE    PARA-JTIME    TO        KEN-F02.
     MOVE    BATTI-JIKOKU  TO        KEN-F02.
     MOVE    HEN-F01       TO        KEN-F03.
     MOVE    WK-SYUKA-BASYO  TO      KEN-F04.
     MOVE    PARA-KSYU     TO        KEN-F05.
     MOVE    PARA-YUSEN    TO        KEN-F06.
     IF  BATTI-CHK  =  SPACE
         MOVE    CNT-KENSU-D   TO    KEN-F10
     ELSE
         MOVE    CNT-KENSU-D2  TO    KEN-F10
     END-IF.
     MOVE    1             TO        KEN-F11.
     WRITE   KEN-REC.
     GO      TO   JSMKENL1-WRT-EXIT.
*
 JSMKENL1-010.
*
     MOVE    PARA-KSYU     TO        KEN-F05.
     MOVE    PARA-YUSEN    TO        KEN-F06.
*****ADD     CNT-KENSU-D   TO        KEN-F10.
     IF  BATTI-CHK  =  SPACE
         ADD     CNT-KENSU-D   TO    KEN-F10
     ELSE
         ADD     CNT-KENSU-D2  TO    KEN-F10
     END-IF.
     ADD     1             TO        KEN-F11.
     REWRITE KEN-REC.
*
 JSMKENL1-WRT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
     DISPLAY "MAISU = " CNT-MAISU UPON CONS.
     IF        CNT-MAISU     >    ZERO
     OR        CNT-MAISU2    >    ZERO
*              出荷場所件数マスタ出力
               PERFORM  JSMKENL1-WRT-SEC
*              当日スケジュールマスタ出力
***************PERFORM   JSMDAYL1-WRT-SEC
     END-IF.
*
     IF        CNT-MAISU     >    ZERO
*              当日スケジュールマスタ出力
               PERFORM   JSMDAYL1-WRT-SEC
     END-IF.
*
*    IF        CNT-MAISU2    >    ZERO
*              当日スケジュールマスタ出力
*              PERFORM   JSMDAYL2-WRT-SEC
*    END-IF.
*
     MOVE      RD-CNT    TO      IN-CNT.
     MOVE      WRT-CNT   TO      OUT-CNT.
     DISPLAY   MSG-IN    UPON CONS.
     DISPLAY   MSG-OUT   UPON CONS.
     DISPLAY   MSG-END   UPON CONS.
*
     CLOSE     BMSHACF   JHSHENL1
               SHOTBL1   MEIMS1
               JHMRUTL1  TOKMS2
               JSMKENL1  JSMDAYL1.
*
*    ＶＬＤＦ出力処理
     IF        CNT-MAISU     >    ZERO
               PERFORM   VLD500-OUTPUT-SEC
     END-IF.
*    IF        CNT-MAISU2    >    ZERO
*              CALL "TWAIT180"
*              PERFORM   VLD502-OUTPUT-SEC
*              MOVE   WK-BATTI-JIKAN-2 TO     PARA-JIKAN2
*    ELSE
*              MOVE   "0000"           TO     PARA-JIKAN2
*    END-IF.
     CLOSE     VLD500.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
****************************************************************
*　　　　　　　当日スケジュールマスタ出力　　　　　　　　　　　*
****************************************************************
 JSMDAYL1-WRT-SEC        SECTION.
*
     MOVE   "JSMDAYL1-WRT-SEC"  TO   S-NAME.
     MOVE    SPACE         TO        TJS-REC.
     INITIALIZE                      TJS-REC.
     MOVE    PARA-JDATE    TO        TJS-F01.
     MOVE    PARA-JTIME    TO        TJS-F02.
     MOVE    HEN-F01       TO        TJS-F03.
     READ    JSMDAYL1
       INVALID
         CONTINUE
       NOT INVALID
         GO  TO   JSMDAYL1-010
     END-READ.
*
     MOVE    SPACE         TO        TJS-REC.
     INITIALIZE                      TJS-REC.
     MOVE    PARA-JDATE    TO        TJS-F01.
     MOVE    PARA-JTIME    TO        TJS-F02.
     MOVE    HEN-F01       TO        TJS-F03.
     MOVE    1             TO        TJS-F04.
     MOVE    CNT-KENSU     TO        TJS-F09.
     MOVE    CNT-MAISU     TO        TJS-F10.
     MOVE    "1"           TO        TJS-F11.
     MOVE    "1"           TO        TJS-F12.
     MOVE    "1"           TO        TJS-F14.
     WRITE   TJS-REC.
     GO      TO   JSMDAYL1-WRT-EXIT.
*
 JSMDAYL1-010.
*
     MOVE    1             TO        TJS-F04.
     MOVE    CNT-KENSU     TO        TJS-F09.
     MOVE    CNT-MAISU     TO        TJS-F10.
     MOVE    "1"           TO        TJS-F11.
     MOVE    "1"           TO        TJS-F12.
     MOVE    "1"           TO        TJS-F14.
     REWRITE TJS-REC.
*
 JSMDAYL1-WRT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　ＶＬＤ５００出力処理                            *
****************************************************************
 VLD500-OUTPUT-SEC       SECTION.
*
     MOVE   "VLD500-OUTPUT-SEC" TO   S-NAME.
     MOVE      SPACE              TO    VLD-REC.
     INITIALIZE                         VLD-REC.
     MOVE      500                TO    VLD-F02.
     MOVE      "NW"               TO    VLD-F03.
     MOVE      52                 TO    VLD-F10.
     MOVE      PARA-JDATE         TO    VLD-F11.
     MOVE      PARA-JTIME         TO    VLD-F12.
     MOVE      HEN-F01            TO    VLD-F13.
     WRITE     VLD-REC.
*
 VLD500-OUTPUT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　当日スケジュールマスタ出力　　　　　　　　　　　*
****************************************************************
 JSMDAYL2-WRT-SEC        SECTION.
*
     MOVE   "JSMDAYL2-WRT-SEC"  TO   S-NAME.
     MOVE    SPACE         TO        TJS-REC.
     INITIALIZE                      TJS-REC.
     MOVE    PARA-JDATE    TO        TJS-F01.
     MOVE    WK-BATTI-JIKAN-2  TO        TJS-F02.
     MOVE    HEN-F01       TO        TJS-F03.
     READ    JSMDAYL1
       INVALID
         CONTINUE
       NOT INVALID
         GO  TO   JSMDAYL2-010
     END-READ.
*
     MOVE    SPACE         TO        TJS-REC.
     INITIALIZE                      TJS-REC.
     MOVE    PARA-JDATE    TO        TJS-F01.
     MOVE    WK-BATTI-JIKAN-2  TO        TJS-F02.
     MOVE    HEN-F01       TO        TJS-F03.
     MOVE    1             TO        TJS-F04.
     MOVE    CNT-KENSU2    TO        TJS-F09.
     MOVE    CNT-MAISU2    TO        TJS-F10.
     MOVE    "1"           TO        TJS-F11.
     MOVE    "1"           TO        TJS-F12.
     MOVE    "1"           TO        TJS-F14.
     WRITE   TJS-REC.
     GO      TO   JSMDAYL2-WRT-EXIT.
*
 JSMDAYL2-010.
*
     MOVE    1             TO        TJS-F04.
     MOVE    CNT-KENSU2    TO        TJS-F09.
     MOVE    CNT-MAISU2    TO        TJS-F10.
     MOVE    "1"           TO        TJS-F11.
     MOVE    "1"           TO        TJS-F12.
     MOVE    "1"           TO        TJS-F14.
     REWRITE TJS-REC.
*
 JSMDAYL2-WRT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　ＶＬＤ５００出力処理                            *
****************************************************************
 VLD502-OUTPUT-SEC       SECTION.
*
     MOVE   "VLD502-OUTPUT-SEC" TO   S-NAME.
     MOVE      SPACE              TO    VLD-REC.
     INITIALIZE                         VLD-REC.
     MOVE      500                TO    VLD-F02.
     MOVE      "NW"               TO    VLD-F03.
     MOVE      52                 TO    VLD-F10.
     MOVE      PARA-JDATE         TO    VLD-F11.
     MOVE      WK-BATTI-JIKAN-2   TO    VLD-F12.
     MOVE      HEN-F01            TO    VLD-F13.
     WRITE     VLD-REC.
*
 VLD502-OUTPUT-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
