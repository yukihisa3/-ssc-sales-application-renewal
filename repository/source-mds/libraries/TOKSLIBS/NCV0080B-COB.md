# NCV0080B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/NCV0080B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　受配信管理システム　　　　　　　　*
*    業務名　　　　　　　：　ＣＶＣＳ管理                      *
*    モジュール名　　　　：　週間スケジュールマスタ作成        *
*    作成日／更新日　　　：　99/09/20                          *
*    作成者／更新者　　　：　ＮＡＶ高橋                        *
*    処理概要　　　　　　：　　　　　　　　　　　　　　　　　　*
*                                                              *
****************************************************************
 IDENTIFICATION        DIVISION.
 PROGRAM-ID.           NCV0080B.
 AUTHOR.               TAKAHASHI.
 DATE-WRITTEN.         99/09/20.
****************************************************************
 ENVIRONMENT           DIVISION.
****************************************************************
 CONFIGURATION         SECTION.
 SPECIAL-NAMES.
     CONSOLE      IS   CONS.
*
 INPUT-OUTPUT          SECTION.
 FILE-CONTROL.
*基本スケジュールマスタ
     SELECT  JSMKIHF   ASSIGN    TO        DA-01-VI-JSMKIHL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      SEQUENTIAL
                       RECORD    KEY       KIH-F01
                                           KIH-F02
                       FILE      STATUS    KIH-ST.
*スポット受信スケジュールマスタ
     SELECT  JSMSPTF   ASSIGN    TO        DA-01-VI-JSMSPTL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      SEQUENTIAL
                       RECORD    KEY       SJS-F02
                                           SJS-F03
                       FILE      STATUS    SJS-ST.
*当日スケジュールマスタ
     SELECT  JSMDAYF   ASSIGN    TO        DA-01-VI-JSMDAYL1
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      RANDOM
                       RECORD    KEY       TJS-F01
                                           TJS-F02
                                           TJS-F03
                       FILE      STATUS    TJS-ST.
*
****************************************************************
 DATA                DIVISION.
****************************************************************
 FILE                SECTION.
****************************************************************
*    FILE = 基本スケジュールマスタ                             *
****************************************************************
 FD  JSMKIHF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      JSMKIHF   OF   XFDLIB
                       JOINING   KIH       AS   PREFIX.
****************************************************************
*    FILE = スポット受信スケジュールマスタ                     *
****************************************************************
 FD  JSMSPTF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      JSMSPTF   OF   XFDLIB
                       JOINING   SJS       AS   PREFIX.
****************************************************************
*    FILE = 当日スケジュールマスタ                             *
****************************************************************
 FD  JSMDAYF
                       LABEL     RECORD    IS   STANDARD.
                       COPY      JSMDAYF   OF   XFDLIB
                       JOINING   TJS       AS   PREFIX.
*
****************************************************************
 WORKING-STORAGE     SECTION.
****************************************************************
*ステータス領域
 01  STATUS-AREA.
     03  KIH-ST                   PIC  X(02).
     03  SJS-ST                   PIC  X(02).
     03  TJS-ST                   PIC  X(02).
*区分、ＦＬＧエリア
 01  FLG-AREA.
     03  END-FLG1                 PIC  X(03)  VALUE  SPACE.
     03  END-FLG2                 PIC  X(03)  VALUE  SPACE.
     03  JSMDAYF-INV-FLG          PIC  X(03)  VALUE  SPACE.
     03  GO-FLG                   PIC  X(02)  VALUE  SPACE.
     03  MATUBI                   PIC  X(24)  VALUE
         "312831303130313130313031".
     03  FILLER                   REDEFINES   MATUBI.
         05  WK-MATUBI            PIC  9(02)  OCCURS  12.
*カウンター
 01  CNT-AREA.
     03  CRT-CNT                  PIC  9(06)  VALUE  ZERO.
     03  SPT-CNT                  PIC  9(06)  VALUE  ZERO.
     03  SKIP-CNT                 PIC  9(06)  VALUE  ZERO.
*システム日付格納
 01  SYS-DATE                     PIC  9(06)  VALUE  ZERO.
*システム日付格納（８桁）
 01  WK-DATE.
     03  WK-YY                    PIC  9(04)  VALUE  ZERO.
     03  WK-MM                    PIC  9(02)  VALUE  ZERO.
     03  WK-DD                    PIC  9(02)  VALUE  ZERO.
*システム日付格納（８桁）
 01  WK-START-DATE.
     03  WK-START-YY              PIC  9(04)  VALUE  ZERO.
     03  WK-START-MM              PIC  9(02)  VALUE  ZERO.
     03  WK-START-DD              PIC  9(02)  VALUE  ZERO.
*システム日付＋７日後
 01  WK-END-DATE.
     03  WK-END-YY                PIC  9(04)  VALUE  ZERO.
     03  WK-END-MM                PIC  9(02)  VALUE  ZERO.
     03  WK-END-DD                PIC  9(02)  VALUE  ZERO.
*システム時間格納
 01  WK-TIME.
     03  WK-TIME-1                PIC  9(04)  VALUE  ZERO.
     03  WK-TIME-2                PIC  9(04)  VALUE  ZERO.
*システム時間格納
 01  WK-YOUBI                     PIC  9(01)  VALUE  ZERO.
*日付加算
 01  WK-HIDUKE                    PIC  9(01)  VALUE  ZERO.
*ブレイク曜日ワーク
 01  BRK-YOUBI                    PIC  9(01)  VALUE  ZERO.
*ファイルエラーメッセージ
 01  FILE-ERR.
     03  KIH-ERR           PIC N(15) VALUE
         NC"基本スケジュールマスタエラー".
     03  SJS-ERR           PIC N(15) VALUE
         NC"スポットスケジュールＭエラー".
     03  TJS-ERR           PIC N(15) VALUE
         NC"当日スケジュールマスタエラー".
***  エラーセクション名
 01  SEC-NAME.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-SEC    => ".
     03  S-NAME                   PIC  X(20).
***  エラーファイル名
 01  ERR-FILE.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-FILE   => ".
     03  E-FILE                   PIC  X(08).
***  エラーステータス名
 01  ERR-NAME.
     03  FILLER                   PIC  X(18)
         VALUE "### ERR-STATUS => ".
     03  E-ST                     PIC  9(02).
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
**************************************************************
 LINKAGE               SECTION.
 01  LINK-CHK              PIC 9(01).
*
**************************************************************
 PROCEDURE             DIVISION  USING  LINK-CHK.
**************************************************************
 DECLARATIVES.
 KIH-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE JSMKIHF.
     MOVE        KIH-ST    TO        E-ST.
     MOVE        "JSMKIHF" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     KIH-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 SJS-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE JSMSPTF.
     MOVE        SJS-ST    TO        E-ST.
     MOVE        "JSMSPTF" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     SJS-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 TJS-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE JSMDAYF.
     MOVE        TJS-ST    TO        E-ST.
     MOVE        "JSMDAYF" TO        E-FILE.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     ERR-FILE  UPON      CONS.
     DISPLAY     ERR-NAME  UPON      CONS.
     DISPLAY     TJS-ERR   UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
 END  DECLARATIVES.
****************************************************************
*             MAIN        MODULE                     0.0       *
****************************************************************
 PROCESS-START         SECTION.
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM   INIT-SEC.
     PERFORM   MAIN-SEC    UNTIL   END-FLG1  =  "END"
                             AND   END-FLG2  =  "END".
     PERFORM   END-SEC.
     STOP  RUN.
 PROCESS-END.
     EXIT.
****************************************************************
*             初期処理                               0.0       *
****************************************************************
 INIT-SEC              SECTION.
     MOVE     "INIT-SEC"     TO   S-NAME.
*ファイルのＯＰＥＮ
     OPEN      INPUT   JSMKIHF.
     OPEN      I-O     JSMSPTF.
     OPEN      I-O     JSMDAYF.
*システム日付取得／時刻取得／曜日取得
     ACCEPT    SYS-DATE    FROM    DATE.
     ACCEPT    WK-TIME     FROM    TIME.
     ACCEPT    WK-YOUBI    FROM    DAY-OF-WEEK.
     DISPLAY "WK-YOUBI = " WK-YOUBI UPON CONS.
*システム日付６桁→８桁変換（サブ日付チェック／変換）
 INIT010.
     MOVE      "3"         TO      LINK-IN-KBN.
     MOVE      SYS-DATE    TO      LINK-IN-YMD6.
     MOVE      ZERO        TO      LINK-IN-YMD8.
     MOVE      ZERO        TO      LINK-OUT-RET.
     MOVE      ZERO        TO      LINK-OUT-YMD.
     CALL     "SKYDTCKB"   USING   LINK-IN-KBN
                                   LINK-IN-YMD6
                                   LINK-IN-YMD8
                                   LINK-OUT-RET
                                   LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD TO     WK-DATE  WK-START-DATE
                                   WK-END-DATE.
*ＰＧスタートメッセージ
     DISPLAY "***NCV0080B START " WK-YY "/" WK-MM "/" WK-DD
         " " WK-TIME-1(1:2) ":" WK-TIME-1(3:2) "***" UPON CONS.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*             メイン処理                             1.0       *
****************************************************************
 MAIN-SEC              SECTION.
     MOVE     "MAIN-SEC"     TO   S-NAME.
*基本スケジュールマスタスタート
     MOVE      WK-YOUBI            TO   KIH-F01.
*パラメタにより本日全てを再作成か、現在時刻を再作成かチェック
     IF        LINK-CHK  =  1
               ACCEPT  WK-TIME  FROM  TIME
               MOVE    WK-TIME(1:4) TO   KIH-F02
     ELSE
               MOVE    ZERO         TO   KIH-F02
     END-IF.
     START     JSMKIHF   KEY  IS  >=    KIH-F01  KIH-F02
               INVALID
               MOVE     "END"      TO   END-FLG1
               NOT  INVALID
               PERFORM   JSMKIHF-READ-SEC
     END-START.
*基本スケジュールマスタから作成
     PERFORM   KIHON-CRT-SEC   UNTIL  END-FLG1  =  "END".
*スポット受信スケジュールマスタスタート
     MOVE      WK-START-DATE       TO   SJS-F02.
     MOVE      ZERO                TO   SJS-F03.
     START     JSMSPTF   KEY  IS   >=   SJS-F02 SJS-F03
               INVALID
               DISPLAY "************************"  UPON CONS
               DISPLAY "*ｺﾝｶｲ ﾉ ｽﾎﾟｯﾄ ｼﾞｭｼﾝ ﾉ  *"  UPON CONS
               DISPLAY "*ﾄｳﾛｸ ﾊ ｱﾘﾏｾﾝﾃﾞｼﾀ      *"  UPON CONS
               DISPLAY "************************"  UPON CONS
               MOVE    "END"             TO     END-FLG2
               NOT  INVALID
               PERFORM   JSMSPTF-READ-SEC
     END-START.
*スポット受信スケジュールマスタから作成
     PERFORM   SPOT-CRT-SEC    UNTIL  END-FLG2  =  "END".
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*             終了処理                               3.0       *
****************************************************************
 END-SEC               SECTION.
     MOVE     "END-SEC"            TO   S-NAME.
*ファイルのＯＰＥＮ
     CLOSE     JSMKIHF  JSMSPTF  JSMDAYF.
*件数メッセージ出力
     DISPLAY   "***ｷﾎﾝｽｹｼﾞｭｰﾙ ｻｸｾｲ ｹﾝｽｳ   = " CRT-CNT  UPON CONS.
     DISPLAY   "***ｽﾎﾟｯﾄｽｹｼﾞｭｰﾙ ｻｸｾｲ ｹﾝｽｳ = " SPT-CNT  UPON CONS.
     DISPLAY   "***ｽｹｼﾞｭｰﾙ ﾄｳﾛｸｽﾞﾐ ｹﾝｽｳ   = " SKIP-CNT UPON CONS.
*ＰＧスタートメッセージ
     ACCEPT    WK-TIME  FROM     TIME.
     DISPLAY "***NCV0080B END   " WK-YY "/" WK-MM "/" WK-DD
         " " WK-TIME-1(1:2) ":" WK-TIME-1(3:2) "***" UPON CONS.
*
 END-EXIT.
     EXIT.
****************************************************************
*             基本スケジュールマスタから作成         2.1       *
****************************************************************
 KIHON-CRT-SEC         SECTION.
     MOVE     "KIHON-CRT-SEC"      TO   S-NAME.
*曜日ブレイクチェック
     IF        BRK-YOUBI   NOT =  KIH-F01
*              システム日付のｎ日後算出
               MOVE      "5"         TO      LINK-IN-KBN
***************MOVE      WK-HIDUKE   TO      LINK-IN-YMD6
               COMPUTE   LINK-IN-YMD6 = KIH-F01 - WK-YOUBI
               MOVE      WK-DATE     TO      LINK-IN-YMD8
               MOVE      ZERO        TO      LINK-OUT-RET
               MOVE      ZERO        TO      LINK-OUT-YMD
               CALL     "SKYDTCKB"   USING   LINK-IN-KBN
                                             LINK-IN-YMD6
                                             LINK-IN-YMD8
                                             LINK-OUT-RET
                                             LINK-OUT-YMD
               MOVE      LINK-OUT-YMD TO     WK-END-DATE
*              ｎ日後カウントアップ
***************ADD       1            TO     WK-HIDUKE
               MOVE      KIH-F01      TO     BRK-YOUBI
     END-IF.
*当日スケジュールマスタ存在チェック
     MOVE     WK-END-DATE          TO   TJS-F01.
     MOVE     KIH-F02              TO   TJS-F02.
     MOVE     KIH-F03              TO   TJS-F03.
     PERFORM  JSMDAYF-READ-SEC.
*作成済か未作成か判定
     IF        JSMDAYF-INV-FLG  =  "INV"
               MOVE    SPACE          TO  TJS-REC
               INITIALIZE                 TJS-REC
               MOVE    WK-END-DATE    TO  TJS-F01
               MOVE    KIH-F02        TO  TJS-F02
               MOVE    KIH-F03        TO  TJS-F03
               MOVE    KIH-F05        TO  TJS-F04
               WRITE   TJS-REC
               ADD     1              TO  CRT-CNT
     ELSE
               DISPLAY "*ｽｹｼﾞｭｰﾙ=ｱﾘ = "
                        WK-END-DATE(1:4) "/" WK-END-DATE(5:2) "/"
                        WK-END-DATE(7:2) " " KIH-F02(1:2) ":"
                        KIH-F02(3:2) UPON CONS
***************DISPLAY "*DATE = " WK-END-DATE   UPON CONS
***************DISPLAY "*TIME = " KIH-F02       UPON CONS
               ADD     1              TO  SKIP-CNT
     END-IF.
*
     PERFORM   JSMKIHF-READ-SEC.
*
 KIHON-CRT-EXIT.
     EXIT.
****************************************************************
*             基本スケジュールマスタ読込み           2.2       *
****************************************************************
 JSMKIHF-READ-SEC      SECTION.
     MOVE     "JSMKIHF-READ-SEC"   TO   S-NAME.
*基本スケジュールマスタ読込み
     READ      JSMKIHF    AT  END
               MOVE     "END"      TO   END-FLG1
     END-READ.
*
 JSMKIHF-READ-EXIT.
     EXIT.
****************************************************************
*             スポット受信スケジュールマスタ読込み   2.3       *
****************************************************************
 SPOT-CRT-SEC          SECTION.
     MOVE     "SPOT-CRT-SEC"       TO   S-NAME.
*対象データ判定
     IF        WK-END-DATE   <   SJS-F02
               MOVE     "END"      TO   END-FLG2
               GO                  TO   SPOT-CRT-EXIT
     END-IF.
*当日スケジュールマスタ存在チェック
     MOVE     SJS-F02              TO   TJS-F01.
     MOVE     SJS-F03              TO   TJS-F02.
     MOVE     SJS-F04              TO   TJS-F03.
     PERFORM  JSMDAYF-READ-SEC.
*作成済か未作成か判定
     IF        JSMDAYF-INV-FLG  =  "INV"
               MOVE    SPACE          TO  TJS-REC
               INITIALIZE                 TJS-REC
               MOVE    SJS-F02        TO  TJS-F01
               MOVE    SJS-F03        TO  TJS-F02
               MOVE    SJS-F04        TO  TJS-F03
               MOVE    SJS-F20        TO  TJS-F04
               WRITE   TJS-REC
               ADD     1              TO  SPT-CNT
     ELSE
               DISPLAY "*ｽｹｼﾞｭｰﾙ(ｽﾎﾟｯﾄ)ｻｸｾｲｽﾐ*"   UPON CONS
               DISPLAY "*DATE  = " SJS-F02        UPON CONS
               DISPLAY "*TIME  = " SJS-F03        UPON CONS
               DISPLAY "*TOKCD = " SJS-F04        UPON CONS
               DISPLAY "*DTKBN = " SJS-F20        UPON CONS
               ADD     1              TO  SKIP-CNT
     END-IF.
*作成区分更新
     MOVE      "2"                    TO  SJS-F01.
     REWRITE    SJS-REC.
*スポット受信スケジュールマスタ読込み
     PERFORM   JSMSPTF-READ-SEC.
*
 SPOT-CRT-EXIT.
     EXIT.
****************************************************************
*             スポット受信スケジュールマスタ読込み   2.2       *
****************************************************************
 JSMSPTF-READ-SEC      SECTION.
     MOVE     "JSMSPTF-READ-SEC"   TO   S-NAME.
*スポット受信スケジュールマスタ読込み
 SJS010.
     READ      JSMSPTF    AT  END
               MOVE     "END"      TO   END-FLG2
               GO                  TO   JSMSPTF-READ-EXIT
     END-READ.
*作成区分チェック
     IF        SJS-F01  =  "2"
               GO                  TO   SJS010
     END-IF.
*
 JSMSPTF-READ-EXIT.
     EXIT.
****************************************************************
*             当日スケジュールマスタ読込み（乱）     2.2       *
****************************************************************
 JSMDAYF-READ-SEC      SECTION.
     MOVE     "JSMDAYF-READ-SEC"   TO   S-NAME.
*当日スケジュールマスタ読込み
     READ      JSMDAYF
               INVALID
               MOVE     "INV"      TO   JSMDAYF-INV-FLG
               NOT  INVALID
               MOVE     SPACE      TO   JSMDAYF-INV-FLG
     END-READ.
*
 JSMSPTF-READ-EXIT.
     EXIT.
*****************<<  NCV0080B   END PROGRAM  >>******************

```
