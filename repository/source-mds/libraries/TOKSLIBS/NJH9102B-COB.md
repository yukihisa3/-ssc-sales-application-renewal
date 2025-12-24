# NJH9102B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/NJH9102B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　出荷管理　　　　　　　　　　　　　*
*    業務名　　　　　　　：　ベンダーオンライン　　　　　　　　*
*    モジュール名　　　　：　変換伝票データ作成（ＥＤＩＣ）　　*
*    作成日／更新日　　　：　15/08/20                          *
*    作成者／更新者　　　：　ＮＡＶ高橋　　　　　　　　　　　　*
*    処理概要　　　　　　：　（ＣＶＣＳ）オンラインデータから　*
*                            変換伝票データファイルを作成する　*
*                            （ＥＤＩＣ用）                    *
*　　　　　　　　　　　　　　基本情報データ作成　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            NJH9102B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          15/08/20.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*受信データファイル
     SELECT   CVCSG001  ASSIGN    TO        DA-01-S-CVCSG001
                        ACCESS    MODE IS   SEQUENTIAL
                        FILE      STATUS    DEN-STATUS
                        ORGANIZATION   IS   SEQUENTIAL.
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
*商品変換テーブル
     SELECT   SHOTBL1   ASSIGN    TO        DA-01-VI-SHOTBL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       TBL-F01   TBL-F02
                        FILE STATUS    IS   TBL-STATUS.
*商品名称マスタ
     SELECT   MEIMS1    ASSIGN    TO        DA-01-VI-MEIMS1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       MEI-F011
                                            MEI-F012
                        FILE STATUS    IS   MEI-STATUS.
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
*ルート条件マスタ
     SELECT   JHMRUTL1  ASSIGN    TO        DA-01-VI-JHMRUTL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       RUT-F01   RUT-F02
                                            RUT-F03
                        FILE STATUS    IS   RUT-STATUS.
*
*ＶＬＤ５００
     SELECT   VLD500    ASSIGN    TO        VLD500
                        FILE  STATUS   IS   VLD-STATUS.
*ＥＤＩＣ発注ＭＳＧファイル（キー１）
     SELECT   EDJOHOF   ASSIGN         DA-01-VI-EDJOHOL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  JOH-F011
                                       JOH-F012
                                       JOH-F013
                                       JOH-F02
                                       JOH-F03
                                       JOH-F04
                                       JOH-F05
                                       JOH-F06
                        FILE STATUS    IS   JOH-STATUS.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    受信データ　ＲＬ＝　３８４　ＢＦ＝　１
******************************************************************
 FD  CVCSG001
                        BLOCK CONTAINS      1    RECORDS
                        LABEL RECORD   IS   STANDARD.
*
 01  CVCSG001-REC.
     03  CVCSG001-F01             PIC   X(004).
     03  CVCSG001-FIL             PIC   X(380).
******************************************************************
*    取引先マスタ
******************************************************************
 FD  TOKMS2             LABEL RECORD   IS   STANDARD.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
******************************************************************
*    商品変換テーブル
******************************************************************
 FD  SHOTBL1            LABEL RECORD   IS   STANDARD.
     COPY     HSHOTBL   OF        XFDLIB
              JOINING   TBL       PREFIX.
******************************************************************
*    商品名称マスタ
******************************************************************
 FD  MEIMS1             LABEL RECORD   IS   STANDARD.
     COPY     HMEIMS    OF        XFDLIB
              JOINING   MEI       PREFIX.
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
*↓2013/06/27
******************************************************************
*    ルート条件マスタ
******************************************************************
 FD  JHMRUTL1           LABEL RECORD   IS   STANDARD.
     COPY     JHMRUTF   OF        XFDLIB
              JOINING   RUT       PREFIX.
*↑2013/06/27
******************************************************************
*    変換伝票データ　ＲＬ＝１０２０
******************************************************************
 FD  JHSHENL1
                        LABEL RECORD   IS   STANDARD.
     COPY     BSHIRED   OF        XFDLIB
              JOINING   HEN  AS   PREFIX.
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
******************************************************************
*ＥＤＩＣ発注ＭＳＧファイル（キー１）
******************************************************************
 FD  EDJOHOF
                        LABEL     RECORD   IS   STANDARD.
     COPY     EDJOHOF   OF        XFDLIB
              JOINING   JOH       PREFIX.
*
*****************************************************************
*
 WORKING-STORAGE        SECTION.
*    ｶｳﾝﾄ
 01  END-FG                  PIC  9(01)     VALUE  ZERO.
 01  IDX                     PIC  9(02)     VALUE  ZERO.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT                 PIC  9(08)     VALUE  ZERO.
 01  CNT-KENSU               PIC  9(08)     VALUE  ZERO.
 01  CNT-KENSU-D             PIC  9(08)     VALUE  ZERO.
 01  CNT-MAISU               PIC  9(08)     VALUE  ZERO.
 01  FLG-TOK                 PIC  9(01)     VALUE  ZERO.
*
     COPY     EDJOHOF   OF        XFDLIB
              JOINING   WJOH      PREFIX.
*
 01  WK-WJOH-F200.
     03  WK-WJOH-F200-1    PIC  X(178).
     03  WK-WJOH-F200-2    PIC  X(020).
     03  WK-WJOH-F200-3    PIC  X(186).
*
 01  WK-WJOH-F400.
     03  WK-WJOH-F400-1    PIC  X(079).
     03  WK-WJOH-F400-2    PIC  X(024).
     03  WK-WJOH-F400-3    PIC  X(026).
     03  WK-WJOH-F400-4    PIC  X(024).
     03  WK-WJOH-F400-5    PIC  X(231).
*
 01  WK-AREA.
*システム日付の編集
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
 01  WK-ST.
     03  DEN-STATUS        PIC  X(02).
     03  HEN-STATUS        PIC  X(02).
     03  TOK-STATUS        PIC  X(02).
     03  TBL-STATUS        PIC  X(02).
     03  MEI-STATUS        PIC  X(02).
     03  KEN-STATUS        PIC  X(02).
     03  TJS-STATUS        PIC  X(02).
     03  RUT-STATUS        PIC  X(02).
     03  VLD-STATUS        PIC  X(02).
     03  JOH-STATUS        PIC  X(02).
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "NJH9102B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NJH9102B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "NJH9102B".
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
*コード変換
**１３桁
 01  WK-13-CD               PIC  X(13).
 01  WK-13-CD-R             REDEFINES   WK-13-CD.
     03  HK-13-CD           PIC  9(13).
**１０桁
 01  WK-10-CD               PIC  X(10).
 01  WK-10-CD-R             REDEFINES   WK-10-CD.
     03  HK-10-CD           PIC  9(10).
**０８桁
 01  WK-08-CD               PIC  X(08).
 01  WK-08-CD-R             REDEFINES   WK-08-CD.
     03  HK-08-CD           PIC  9(08).
**０４桁
 01  WK-04-CD               PIC  X(04).
 01  WK-04-CD-R             REDEFINES   WK-04-CD.
     03  HK-04-CD           PIC  9(04).
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
     03  PARA-KSYU          PIC   X(01).
     03  PARA-YUSEN         PIC   X(01).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING PARA-AREA.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   CVCSG001.
     MOVE      "CVCSG001"   TO   AB-FILE.
     MOVE      DEN-STATUS   TO   AB-STS.
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
 FILEERR-SEC8           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   VLD500.
     MOVE      "VLD500  "   TO   AB-FILE.
     MOVE      VLD-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
 FILEERR-SEC9           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   JHMRUTL1.
     MOVE      "JHMRUTL1"   TO   AB-FILE.
     MOVE      RUT-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
 FILEERR-SEC10          SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   EDJOHOF.
     MOVE      "EDJOHOL1"   TO   AB-FILE.
     MOVE      JOH-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
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
              UNTIL     END-FG    =    9.
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     CVCSG001
                        SHOTBL1   MEIMS1
                        TOKMS2.
     OPEN     INPUT     JHMRUTL1.
     OPEN     EXTEND    JHSHENL1.
     OPEN     I-O       JSMKENL1  JSMDAYL1   EDJOHOF.
     OPEN     OUTPUT    VLD500.
     DISPLAY  MSG-START UPON CONS.
*
     MOVE     ZERO      TO        END-FG    RD-CNT    WRT-CNT.
     MOVE     ZERO      TO        IN-CNT    OUT-CNT.
     MOVE     SPACE     TO        WJOH-REC.
     INITIALIZE                   WJOH-REC.
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
*
     READ     CVCSG001
              AT END    MOVE      9         TO  END-FG
              NOT AT END
                        ADD       1         TO  RD-CNT
     END-READ.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"          TO   S-NAME.
*ファイルヘッダー
     IF    CVCSG001-F01  =  "1000"
           MOVE      SPACE         TO    WJOH-F100
           INITIALIZE                    WJOH-F100
           MOVE      CVCSG001-REC  TO    WJOH-F100
     END-IF.
*伝票ヘッダー１
     IF    CVCSG001-F01  =  "1010"
*          出荷場所件数マスタ出力
           IF   CNT-MAISU     >    ZERO
                PERFORM  JSMKENL1-WRT-SEC
           END-IF
           MOVE      SPACE         TO    WJOH-F200
           INITIALIZE                    WJOH-F200
***********MOVE      CVCSG001-REC  TO    WJOH-F200
           MOVE      CVCSG001-REC  TO    WK-WJOH-F200
           MOVE    WK-WJOH-F200-1  TO    WJOH-F200(1:178)
           MOVE    X"28"           TO    WJOH-F200(179:1)
           MOVE    WK-WJOH-F200-2  TO    WJOH-F200(180:20)
           MOVE    X"29"           TO    WJOH-F200(200:1)
           MOVE    WK-WJOH-F200-3  TO    WJOH-F200(201:184)
           ADD       1        TO   CNT-MAISU
           MOVE      ZERO     TO   CNT-KENSU-D
     END-IF.
*伝票ヘッダー２
     IF    CVCSG001-F01  =  "1011"
           MOVE      SPACE         TO    WJOH-F300
           INITIALIZE                    WJOH-F300
           MOVE      CVCSG001-REC  TO    WJOH-F300
     END-IF.
*明細行
     IF    CVCSG001-F01  =  "1020"
           MOVE      SPACE         TO    WJOH-F400
           INITIALIZE                    WJOH-F400
***********MOVE      CVCSG001-REC  TO    WJOH-F400
           MOVE      CVCSG001-REC  TO    WK-WJOH-F400
           MOVE    WK-WJOH-F400-1  TO    WJOH-F400(1:79)
           MOVE    X"28"           TO    WJOH-F400(80:1)
           MOVE    WK-WJOH-F400-2  TO    WJOH-F400(81:24)
           MOVE    X"29"           TO    WJOH-F400(105:1)
           MOVE    WK-WJOH-F400-3  TO    WJOH-F400(106:26)
           MOVE    X"28"           TO    WJOH-F400(132:1)
           MOVE    WK-WJOH-F400-4  TO    WJOH-F400(133:24)
           MOVE    X"29"           TO    WJOH-F400(157:1)
           MOVE    WK-WJOH-F400-5  TO    WJOH-F400(158:227)
           ADD       1        TO   CNT-KENSU
           ADD       1        TO   CNT-KENSU-D
     END-IF
*
     IF    CVCSG001-F01  =  "1020"
           PERFORM                       EDIT-SEC
     END-IF
*
     READ     CVCSG001
              AT END    MOVE      9         TO  END-FG
              NOT AT END
                        ADD       1         TO  RD-CNT
     END-READ.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　ファイル出力　　　　　　　　　　　　　　　　　　*
****************************************************************
 EDIT-SEC              SECTION.
*
     MOVE    "EDIT-SEC"     TO        S-NAME.
*ＥＤＩＣ発注ＭＳＧファイル作成
*    初期化
     MOVE     SPACE         TO        JOH-REC  HEN-REC.
     INITIALIZE                       JOH-REC  HEN-REC.
*    バッチ日付
     MOVE     PARA-JDATE    TO        JOH-F011.
*    バッチ時刻
     MOVE     PARA-JTIME    TO        JOH-F012.
*    バッチ取引先
     MOVE     WJOH-F107     TO        WK-13-CD.
     MOVE     HK-13-CD      TO        JOH-F013.
*    ファイルヘッダ転送
     MOVE     WJOH-F100     TO        JOH-F100.
*    伝票ヘッダー１
     MOVE     WJOH-F200     TO        JOH-F200.
*    伝票ヘッダー２
     MOVE     WJOH-F300     TO        JOH-F300
*    伝票明細
     MOVE     WJOH-F400     TO        JOH-F400
*
     PERFORM  TENSO-SEC.
*
     WRITE    JOH-REC.
     WRITE    HEN-REC.
     ADD      1             TO   WRT-CNT.
*
 EDIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　変換伝票転送                                    *
****************************************************************
 TENSO-SEC             SECTION.
*
     MOVE    "TENSO-SEC"   TO        S-NAME.
*取引先コード
     MOVE     WJOH-F107    TO        WK-13-CD.
     MOVE     HK-13-CD     TO        HEN-F01.
*伝票ナンバー
     MOVE     WJOH-F208    TO        WK-10-CD.
     MOVE     HK-10-CD     TO        HEN-F02.
     MOVE     HEN-F02      TO        HEN-F23.
     MOVE     HEN-F02      TO        JOH-F05.
*行番号
     MOVE     WJOH-F402    TO        WK-04-CD.
     MOVE     HK-04-CD     TO        HEN-F03.
     MOVE     HK-04-CD     TO        JOH-F06.
*取区
     MOVE        40        TO        HEN-F051.
     MOVE     NC"売上伝票" TO        HEN-F052.
*担当者コード
     MOVE        99        TO        HEN-F06.
*店コード
     MOVE     WJOH-F213    TO        WK-13-CD.
     MOVE     HK-13-CD     TO        HEN-F07  JOH-F03.
*  商品変換テーブル検索
     MOVE     SPACE        TO        TBL-REC.
     INITIALIZE                      TBL-REC.
     MOVE     HEN-F01      TO        TBL-F01.
     MOVE     WJOH-F408(2:13) TO     TBL-F02.
     READ    SHOTBL1
       INVALID
         MOVE    SPACE     TO        TBL-REC
         INITIALIZE                  TBL-REC
     END-READ.
*出荷場所／伝発場所
     MOVE        TBL-F04   TO        HEN-F08
                                     HEN-F09
                                     JOH-F02.
*発注日  (年を条件ファイルの置換値とプラスして西暦４桁にする）
     MOVE     WJOH-F304    TO        WK-08-CD.
     MOVE     HK-08-CD     TO        HEN-F111.
*納品日  (年を条件ファイルの置換値とプラスして西暦４桁にする）
     MOVE     WJOH-F306    TO        WK-08-CD.
     MOVE     HK-08-CD     TO        HEN-F112  JOH-F04.
*分類（部門）
     MOVE     WJOH-F302(8:3) TO      HEN-F12.
*商品区分　　
     MOVE     SPACE        TO        HEN-F131.
*伝票区分　　
     MOVE     WJOH-F310    TO        HEN-F132.
*伝発区分
     MOVE     9            TO        HEN-F134.
*自社商品コード
     MOVE     TBL-F031     TO        HEN-F1411.
*自社商品単品コード
     MOVE     TBL-F032     TO        HEN-F1412.
*商品名　　　
*    商品名称マスタ検索
     MOVE     SPACE        TO        MEI-REC.
     INITIALIZE                      MEI-REC.
     MOVE     TBL-F031     TO        MEI-F011.
     MOVE     TBL-F032     TO        MEI-F012.
     READ    MEIMS1
       INVALID
*       商品名
        MOVE  WJOH-F412(1:15)  TO    HEN-F1421
        MOVE  WJOH-F412(16:10) TO    HEN-F1422(1:10)
        MOVE  WJOH-F414(1:5)   TO    HEN-F1422(11:5)
       NOT INVALID
        MOVE  WJOH-F412(1:15)  TO    HEN-F1421
        MOVE  WJOH-F412(16:10) TO    HEN-F1422(1:10)
        MOVE  WJOH-F414(1:5)   TO    HEN-F1422(11:5)
     END-READ.
*数量
     COMPUTE  HEN-F15  =  WJOH-F421  /  10.
*単
     MOVE       "1"        TO        HEN-F16.
*原価単価
     COMPUTE  HEN-F172 =  WJOH-F417  /  100.
*売価単価
     COMPUTE  HEN-F173 =  WJOH-F419  /  100.
*原価金額
     COMPUTE     HEN-F181 = HEN-F15 *  HEN-F172.
*売価金額
     COMPUTE     HEN-F182 = HEN-F15 *  HEN-F173.
*店舗名（備考）
*自社得意先コード
     IF  FLG-TOK   =    ZERO
*    得意先マスタ検索
         MOVE    SPACE         TO    TOK-REC
         INITIALIZE                  TOK-REC
         MOVE    HEN-F01       TO    TOK-F01
         READ    TOKMS2
             INVALID
               MOVE  SPACE     TO    TOK-REC
               INITIALIZE            TOK-REC
         END-READ
         MOVE    TOK-F52   TO        HEN-F24
         MOVE    1         TO        FLG-TOK
     ELSE
         MOVE    TOK-F52   TO        HEN-F24
     END-IF.
*相手商品コード
     MOVE    WJOH-F408(2:13) TO      HEN-F25.
*伝票発行区分
     MOVE        9         TO        HEN-F272.
*オンライン区分
     MOVE        1         TO        HEN-F274.
*エントリー区分
     MOVE        1         TO        HEN-F275.
*付番区分
     MOVE        9         TO        HEN-F276.
*量販店区分
*****MOVE       "D"        TO        HEN-F278.
     MOVE       "A"        TO        HEN-F278.
*ＷＳ_
     MOVE        1         TO        HEN-F28.
*変換値
*****MOVE        WK-NEN    TO        HEN-F29.
*店舗名カナ
     MOVE     WJOH-F216    TO        HEN-F30.
*システム日付
     MOVE     SYS-DATEW    TO        HEN-F99.
*受信日付
     MOVE     PARA-JDATE   TO        HEN-F46.
*受信時刻
     MOVE     PARA-JTIME   TO        HEN-F47.
*ルート
     MOVE     WJOH-F230    TO        HEN-F42.
*振分倉庫コード
*    ルート条件マスタ検索
     MOVE     HEN-F01      TO        RUT-F01.
     MOVE     SPACE        TO        RUT-F02.
     MOVE     HEN-F42      TO        RUT-F03.
     READ     JHMRUTL1
         INVALID
           MOVE  TOK-F81   TO        HEN-F48  JOH-F02
         NOT INVALID
           MOVE  RUT-F05   TO        HEN-F48  JOH-F02
     END-READ.
*タナ番
     MOVE        TBL-F08   TO        HEN-F49.
*訂正前数量
     MOVE        HEN-F15   TO        HEN-F50.
*修正原価単価
     MOVE        HEN-F172  TO        HEN-F512.
*修正売価単価
     MOVE        HEN-F173  TO        HEN-F513.
*修正原価金額
     MOVE        HEN-F181  TO        HEN-F521.
*修正売価金額
     MOVE        HEN-F182  TO        HEN-F522.
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
     MOVE    PARA-JDATE    TO        KEN-F01.
     MOVE    PARA-JTIME    TO        KEN-F02.
     MOVE    HEN-F01       TO        KEN-F03.
     MOVE    HEN-F48       TO        KEN-F04.
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
     MOVE    PARA-JTIME    TO        KEN-F02.
     MOVE    HEN-F01       TO        KEN-F03.
     MOVE    HEN-F48       TO        KEN-F04.
     MOVE    PARA-KSYU     TO        KEN-F05.
     MOVE    PARA-YUSEN    TO        KEN-F06.
     MOVE    CNT-KENSU-D   TO        KEN-F10.
     MOVE    1             TO        KEN-F11.
     WRITE   KEN-REC.
     GO      TO    JSMKENL1-WRT-EXIT.
*
 JSMKENL1-010.
*
     MOVE    PARA-KSYU     TO        KEN-F05.
     MOVE    PARA-YUSEN    TO        KEN-F06.
     ADD     CNT-KENSU-D   TO        KEN-F10.
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
     IF        CNT-MAISU     >    ZERO
*              出荷場所件数マスタ出力
               PERFORM  JSMKENL1-WRT-SEC
*              当日スケジュールマスタ出力
               PERFORM  JSMDAYL1-WRT-SEC
     END-IF.
*
     MOVE      RD-CNT    TO      IN-CNT.
     MOVE      WRT-CNT   TO      OUT-CNT.
     DISPLAY   MSG-IN    UPON CONS.
     DISPLAY   MSG-OUT   UPON CONS.
     DISPLAY   MSG-END   UPON CONS.
*
     CLOSE     CVCSG001  JHSHENL1
               SHOTBL1   MEIMS1
               TOKMS2
               JSMKENL1  JSMDAYL1
               JHMRUTL1  EDJOHOF.
*
*    ＶＬＤＦ出力処理
     IF        CNT-MAISU     >    ZERO
               PERFORM   VLD500-OUTPUT-SEC
     END-IF.
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
     MOVE    CNT-KENSU     TO        TJS-F09.
     MOVE    CNT-MAISU     TO        TJS-F10.
     WRITE   TJS-REC.
     GO      TO    JSMDAYL1-WRT-EXIT.
*
 JSMDAYL1-010.
*
     MOVE    CNT-KENSU     TO        TJS-F09.
     MOVE    CNT-MAISU     TO        TJS-F10.
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
*-------------< PROGRAM END >------------------------------------*

```
