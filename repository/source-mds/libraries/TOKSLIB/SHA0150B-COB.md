# SHA0150B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SHA0150B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　自動発注システム　　　　　　　　　*
*    モジュール名　　　　：　自動発注確定前処理                *
*    作成日／更新日　　　：　00/07/13                          *
*    作成者／更新者　　　：　ＮＡＶ                            *
*    処理概要　　　　　　：　自動発注ワークを倉庫＋仕入先順に　*
*                          読み、ＭＡＸ６件毎に条件マスタより、*
*                          発注_を取得し、自動発注ワークへ更　*
*                          新するとともに、自動発注ヘッダ、明  *
*                          細へ出力する。                      *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SHA0150B.
 AUTHOR.                NAV.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM-K150.
 OBJECT-COMPUTER.       FACOM-K150.
 SPECIAL-NAMES.
         STATION   IS   STAT
         YA        IS   NIHONGO
         YA-21     IS   YA-21
         YB        IS   YB
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
***-<<  画面ファイル  >>************************************
     SELECT   DSPF      ASSIGN    TO        GS-DSPF
                        ORGANIZATION        IS   SEQUENTIAL
                        ACCESS    MODE      IS   SEQUENTIAL
                        SYMBOLIC  DESTINATION    IS  "DSP"
                        PROCESSING MODE     IS   DSP-PROC
                        GROUP               IS   DSP-GROUP
                        FORMAT              IS   DSP-FORMAT
                        SELECTED  FUNCTION  IS   DSP-FUNC
                        FILE      STATUS    IS   DSP-STATUS.
*
****<<  自動発注ワーク  >>***********************************
     SELECT   AUTHACF   ASSIGN    TO        DA-01-VI-AUTHACL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   SEQUENTIAL
                        RECORD    KEY       IS   AUT-F01
                                                 AUT-F02
                                                 AUT-F031
                                                 AUT-F032
                                                 AUT-F04
                        FILE      STATUS    IS   AUT-STATUS.
*
***-<<  自動発注（ヘッダ）  >>*************************
     SELECT   AUTHEDF   ASSIGN    TO        DA-01-VI-AUTHEDL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   AUH-F02
                        FILE      STATUS    IS   AUH-STATUS.
*
***-<<  自動発注（明細）    >>*************************
     SELECT   AUTMEIF   ASSIGN    TO        DA-01-VI-AUTMEIL1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   AUM-F02
                                                 AUM-F03
                        FILE      STATUS    IS   AUM-STATUS.
*
****<<  条件ファイル    >>**********************************
     SELECT   JYOKEN    ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   JYO-F01
                                                 JYO-F02
                        FILE      STATUS    IS   JYO-STATUS.
*
***
 DATA                   DIVISION.
 FILE                   SECTION.
*
****<<  画面ファイル  >>************************************
 FD  DSPF.
     COPY     FHA01501  OF        XMDLIB
              JOINING   DSP       PREFIX.
****<< 自動発注ワーク   >>**********************************
 FD  AUTHACF.
     COPY     AUTHACF   OF        XFDLIB
              JOINING   AUT       PREFIX.
****<< 自動発注（ヘッダ） >>****************************
 FD  AUTHEDF.
     COPY     AUTHEDF   OF        XFDLIB
              JOINING   AUH       PREFIX.
****<< 自動発注（明細）   >>****************************
 FD  AUTMEIF.
     COPY     AUTMEIF   OF        XFDLIB
              JOINING   AUM       PREFIX.
****<< 条件ファイル         >>******************************
 FD  JYOKEN.
     COPY     JYOKEN1   OF        XFDLIB
              JOINING   JYO       PREFIX.
****  作業領域  ********************************************
 WORKING-STORAGE        SECTION.
****  画面制御項目  ****
 01  DSP-CONTROL.
     03  DSP-PROC            PIC  X(02).
     03  DSP-GROUP           PIC  X(08).
     03  DSP-FORMAT          PIC  X(08).
     03  DSP-STATUS          PIC  X(02).
     03  DSP-FUNC            PIC  X(04).
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 AUT-STATUS           PIC  X(2).
     02 AUH-STATUS           PIC  X(2).
     02 AUM-STATUS           PIC  X(2).
     02 JYO-STATUS           PIC  X(2).
**** 日付／時刻
 01  TIME-AREA.
     03  WK-TIME                  PIC  9(08)  VALUE  ZERO.
 01  DATE-AREA.
     03  WK-YS                    PIC  9(02)  VALUE  ZERO.
     03  WK-DATE.
         05  WK-Y                 PIC  9(02)  VALUE  ZERO.
         05  WK-M                 PIC  9(02)  VALUE  ZERO.
         05  WK-D                 PIC  9(02)  VALUE  ZERO.
 01  DATE-AREAR2       REDEFINES      DATE-AREA.
     03  SYS-DATE                 PIC  9(08).
**** 画面表示日付編集
 01  HEN-DATE.
     03  HEN-DATE-YYYY            PIC  9(04)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-DD              PIC  9(02)  VALUE  ZERO.
**** 画面表示時刻編集
 01  HEN-TIME.
     03  HEN-TIME-HH              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-SS              PIC  9(02)  VALUE  ZERO.
*特販部名称編集
 01  HEN-TOKHAN-AREA.
     03  FILLER                   PIC  N(01)  VALUE  NC"（".
     03  HEN-TOKHAN               PIC  N(06)  VALUE  SPACE.
     03  FILLER                   PIC  N(01)  VALUE  NC"）".
*
 01  KEY-AREA.
     03  CUR-KEY.
       05  CUR-SOKCD         PIC  X(02)  VALUE  SPACE.
       05  CUR-SIIRE         PIC  X(08)  VALUE  SPACE.
     03  BRK-KEY.
       05  BRK-SOKCD         PIC  X(02)  VALUE  SPACE.
       05  BRK-SIIRE         PIC  X(08)  VALUE  SPACE.
*
****  カウンタ                ****
 01  CNT-IN                  PIC  9(07)  VALUE  ZERO.
 01  CNT-SEL                 PIC  9(07)  VALUE  ZERO.
 01  CNT-AUH                 PIC  9(07)  VALUE  ZERO.
 01  CNT-AUM                 PIC  9(07)  VALUE  ZERO.
****  フラグ                  ****
 01  PFK-FLG                 PIC  9(01)  VALUE  ZERO.
 01  ERR-FLG                 PIC  9(01)  VALUE  ZERO.
 01  AUT-FLG                 PIC  X(03)  VALUE  SPACE.
 01  MAIN-FLG                PIC  9(02)  VALUE  ZERO.
 01  END-FLG                 PIC  X(03)  VALUE  SPACE.
 01  WK-HACNO                PIC  9(07)  VALUE  ZERO.
****  インデックス            ****
 01  IXA                     PIC  9(02).
*
*メッセージテーブル
 01  MSG-TBL.
     03  MSG-NO01            PIC  N(20)  VALUE
            NC"誤ったＰＦキーが押されました".
     03  MSG-NO02            PIC  N(20)  VALUE
            NC"自動発注処理が開始されていません".
     03  MSG-NO03            PIC  N(20)  VALUE
            NC"＊＊＊　自動発注確定前処理中です。＊＊＊".
 01  TBL-MSG-R     REDEFINES      MSG-TBL.
     03  TBL-MSG             PIC  N(20)  OCCURS  3   TIMES.
*ＰＦキー
 01  PFK-TBL.
     03  PFK-NO01            PIC  N(30)  VALUE
            NC"_終了".
 01  TBL-PFK-R     REDEFINES      PFK-TBL.
     03  TBL-PFK             PIC  N(30)  OCCURS  1   TIMES.
****  日付保存                ****
 01  SYSTEM-HIZUKE.
     02  SYSYMD              PIC  9(06).
     02  SYSYMD-R            REDEFINES SYSYMD.
       03  SYS-YY            PIC  99.
       03  SYS-MM            PIC  99.
       03  SYS-DD            PIC  99.
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SHA0150B".
       03  FILLER            PIC  X(10)  VALUE
          " ABEND ###".
     02  MSG-ABEND2.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-FL-ID         PIC  X(08).
       03  FILLER            PIC  X(04)  VALUE  " ST-".
       03  ERR-STCD          PIC  X(02).
       03  FILLER            PIC  X(04)  VALUE  " ###".
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
***************************************************************
 PROCEDURE               DIVISION.
***************************************************************
*
 DECLARATIVES.
 FILEERR-DSP            SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  DSPF.
     MOVE   "DSPF    "        TO    ERR-FL-ID.
     MOVE    DSP-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-AUT            SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  AUTHACF.
     MOVE   "AUTHACF "        TO    ERR-FL-ID.
     MOVE    AUT-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
*
 FILEERR-AUH            SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  AUTHEDF.
     MOVE   "AUTHEDF "        TO    ERR-FL-ID.
     MOVE    AUH-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-AUM            SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  AUTMEIF.
     MOVE   "HACMEIF "        TO    ERR-FL-ID.
     MOVE    AUM-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-JYO            SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  JYOKEN.
     MOVE   "JYOKEN  "        TO    ERR-FL-ID.
     MOVE    JYO-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 END     DECLARATIVES.
************************************************************
 SHA0150B-START         SECTION.
     DISPLAY  "**  SHA0150B   START  **"   UPON  CONS.
*
     PERFORM       INIT-SEC.
     PERFORM       MAIN-SEC     UNTIL   MAIN-FLG = 99.
     PERFORM       END-SEC.
*
     DISPLAY  "**  SHA0150B   END    **"   UPON  CONS.
     STOP     RUN.
 SHA0150B-END.
     EXIT.
************************************************************
*      1.0      初期処理                                   *
************************************************************
 INIT-SEC               SECTION.
     OPEN     I-O       DSPF     JYOKEN    AUTHACF.
     OPEN     OUTPUT    AUTHEDF  AUTMEIF.
     ACCEPT   SYSYMD    FROM     DATE.
*システム日付・時刻の取得
     ACCEPT   WK-DATE           FROM   DATE.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     WK-DATE             TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE     LINK-OUT-YMD        TO   DATE-AREA.
*画面表示日付編集
     MOVE     SYS-DATE(1:4)       TO   HEN-DATE-YYYY.
     MOVE     SYS-DATE(5:2)       TO   HEN-DATE-MM.
     MOVE     SYS-DATE(7:2)       TO   HEN-DATE-DD.
*システム日付取得
     ACCEPT   WK-TIME           FROM   TIME.
*画面表示時刻編集
     MOVE     WK-TIME(1:2)        TO   HEN-TIME-HH.
     MOVE     WK-TIME(3:2)        TO   HEN-TIME-MM.
     MOVE     WK-TIME(5:2)        TO   HEN-TIME-SS.
*特販部名称編集
     MOVE     SPACE               TO   JYO-REC.
     INITIALIZE                        JYO-REC.
     MOVE    "99"                 TO   JYO-F01.
     MOVE    "BUMON"              TO   JYO-F02.
     READ     JYOKEN
       INVALID KEY
              MOVE NC"＊＊＊＊＊＊"    TO   HEN-TOKHAN
       NOT INVALID KEY
              MOVE JYO-F03             TO   HEN-TOKHAN
     END-READ.
     REWRITE  JYO-REC.
*初期画面表示へ
     MOVE     1         TO   MAIN-FLG.
 INIT-END.
     EXIT.
************************************************************
*      2.0       メイン処理（全体）                        *
************************************************************
 MAIN-SEC               SECTION.
     EVALUATE  MAIN-FLG
*初期画面表示
         WHEN      1    PERFORM   DSP-INIT-SEC
*確認入力
         WHEN      2    PERFORM   DSP-KAKU-SEC
*更新処理
         WHEN      3    PERFORM   KOSHIN-SEC
     END-EVALUATE.
 MAIN-EXIT.
     EXIT.
************************************************************
*      3.0        終了処理                                 *
************************************************************
 END-SEC                SECTION.
     CLOSE    DSPF      AUTHEDF   AUTMEIF   AUTHACF
              JYOKEN.
*
     DISPLAY "* AUTHACF (IN)=" CNT-IN   " *" UPON CONS.
     DISPLAY "*         (SL)=" CNT-SEL  " *" UPON CONS.
     DISPLAY "* AUTHEDF (OT)=" CNT-AUH  " *" UPON CONS.
     DISPLAY "* AUTMEIF (OT)=" CNT-AUM  " *" UPON CONS.
*
 END-END.
     EXIT.
**************************************************************
*    2.1         画面初期表示処理
**************************************************************
 DSP-INIT-SEC       SECTION.
*初期画面の処理
     MOVE     SPACE     TO   DSP-CONTROL.
     MOVE     SPACE     TO   DSP-FHA01501.
*確認入力へ
     MOVE     2         TO   MAIN-FLG.
 DSP-INIT-EXIT.
     EXIT.
************************************************************
*    2.2         確認入力処理
************************************************************
 DSP-KAKU-SEC       SECTION.
*画面表示
     MOVE  1                     TO  PFK-FLG.
     PERFORM  DSP-WRITE-SEC.
*画面入力
     MOVE     "KAKU"            TO  DSP-GROUP.
     PERFORM  DSP-READ-SEC.
*ＰＦ判定
     EVALUATE  DSP-FUNC
         WHEN "E000"
                          PERFORM  CHK-KAKU-SEC
         WHEN "F005"
                          MOVE  99      TO  MAIN-FLG
         WHEN OTHER
                          MOVE  1       TO   ERR-FLG
     END-EVALUATE.
*
 DSP-KAKU-EXIT.
     EXIT.
************************************************************
*    2.2.1   確認入力チェック                              *
************************************************************
 CHK-KAKU-SEC   SECTION.
     MOVE  ZERO      TO      ERR-FLG.
*    自動発注区分チェック（１になっているか）
     MOVE    SPACE               TO    JYO-REC.
     INITIALIZE                        JYO-REC.
     MOVE    "82"                TO    JYO-F01.
     MOVE    "JIDO"              TO    JYO-F02.
     READ    JYOKEN
       INVALID KEY
             DISPLAY "JYOKEN1 INV 82 JIDO" UPON CONS
             MOVE       99        TO   MAIN-FLG
             GO    TO   CHK-KAKU-EXIT
     END-READ.
*
     IF  JYO-F04   NOT =     1
         MOVE      2    TO   ERR-FLG
     END-IF.
*
     IF  ERR-FLG  =  ZERO
         MOVE     3     TO   ERR-FLG
         PERFORM  DSP-WRITE-SEC
         MOVE     3     TO   MAIN-FLG
     END-IF.
 CHK-KAKU-EXIT.
     EXIT.
************************************************************
*    2.3        更新処理                                   *
************************************************************
 KOSHIN-SEC             SECTION.
*    自動発注ワーク読み込み
     PERFORM  AUT-READ-SEC.
*
     PERFORM  HENSYU-SEC     UNTIL     AUT-FLG  =   "END".
*
     MOVE     99        TO   MAIN-FLG.
*
 KOSHIN-END.
     EXIT.
************************************************************
*    2.3.1       自動発注ワークの検索                      *
************************************************************
 AUT-READ-SEC           SECTION.
*
     READ     AUTHACF
         AT END
              MOVE     "END"      TO   AUT-FLG
         NOT AT END
              ADD  1    TO   CNT-IN
              IF   AUT-F17   NOT =    "1"
                   GO   TO   AUT-READ-SEC
              ELSE
                   ADD  1    TO   CNT-SEL
                   MOVE AUT-F01   TO   CUR-SOKCD
                   MOVE AUT-F02   TO   CUR-SIIRE
              END-IF
     END-READ.
*
 AUT-READ-END.
     EXIT.
************************************************************
*    2.3.2    自動発注出力編集処理                         *
************************************************************
 HENSYU-SEC             SECTION.
*
     ADD      1         TO   IXA.
     IF       IXA       >    6
              MOVE      1    TO   IXA
     END-IF.
*
     IF  CUR-KEY   =    BRK-KEY
         IF   IXA  =    1
*             発注_採番
              PERFORM   HACNO-GET-SEC
*             自動発注ヘッダワーク出力
              PERFORM   AUTHEDF-WRITE-SEC
*             自動発注明細ワーク出力
              PERFORM   AUTMEIF-WRITE-SEC
*             自動発注ワーク更新
**************MOVE      WK-HACNO       TO   AUT-F15
**************MOVE      IXA            TO   AUT-F16
**************REWRITE   AUT-REC
         ELSE
*             自動発注明細ワーク出力
              PERFORM   AUTMEIF-WRITE-SEC
         END-IF
     ELSE
         MOVE      1    TO   IXA
         MOVE      CUR-KEY   TO   BRK-KEY
*        発注_採番
         PERFORM   HACNO-GET-SEC
*        自動発注ヘッダワーク出力
         PERFORM   AUTHEDF-WRITE-SEC
*        自動発注明細ワーク出力
         PERFORM   AUTMEIF-WRITE-SEC
     END-IF.
*    自動発注ワーク更新
     MOVE      WK-HACNO       TO   AUT-F15.
     MOVE      IXA            TO   AUT-F16.
     REWRITE   AUT-REC.
*
*    自動発注ワーク読み込み
     PERFORM  AUT-READ-SEC.
*
 HENSYU-EXIT.
     EXIT.
************************************************************
*      2.3.2.1   発注_採番処理                            *
************************************************************
 HACNO-GET-SEC          SECTION.
*
     MOVE     60             TO   JYO-F01.
     MOVE     SPACE          TO   JYO-F02.
     READ     JYOKEN
         INVALID
              DISPLAY "JYOKEN1 READ ERR KEY=60+SPACE"
                             UPON CONS
              STOP RUN
     END-READ.
     IF       JYO-F04   =    9999999
              MOVE      ZERO      TO   JYO-F04
     END-IF.
     ADD      1              TO   JYO-F04.
     IF       JYO-F04   =    ZERO
              MOVE      1    TO   JYO-F04
     END-IF.
     MOVE     JYO-F04        TO   WK-HACNO.
     REWRITE  JYO-REC.
*
*    採番発注_存在チェック
**   PERFORM  HAH-READ-SUB.
**   IF  INVALID-FLG   =   ZERO
**       GO   TO   HACNO-SET-SUB
**   ELSE
**       MOVE      ZERO      TO   INVALID-FLG
**   END-IF.
*
 HACNO-GET-EXIT.
     EXIT.
************************************************************
*      画面表示処理
************************************************************
 DSP-WRITE-SEC          SECTION.
*ＰＦキー設定
     MOVE  TBL-PFK(PFK-FLG)  TO   DSP-PFKEY.
*エラー設定
     IF  ERR-FLG        =    ZERO
         MOVE    SPACE       TO   DSP-ERRMSG
     ELSE
         MOVE    TBL-MSG(ERR-FLG)  TO   DSP-ERRMSG
         MOVE    ZERO        TO   ERR-FLG
     END-IF.
*
     MOVE     HEN-DATE       TO   DSP-SDATE.
     MOVE     HEN-TIME       TO   DSP-STIME.
     MOVE     HEN-TOKHAN-AREA     TO   DSP-TOKHAN.
*
     MOVE    "FHA01501"      TO   DSP-FORMAT.
     MOVE    "SCREEN"        TO   DSP-GROUP.
     MOVE     SPACE          TO   DSP-PROC.
     WRITE    DSP-FHA01501.
 DSP-WRITE-END.
     EXIT.
************************************************************
*      画面データの入力処理
************************************************************
 DSP-READ-SEC           SECTION.
     MOVE  "NE"      TO   DSP-PROC.
     READ   DSPF.
*
 DSP-READ-END.
     EXIT.
************************************************************
*      2.3.2.2    自動発注ヘッダワーク出力                 *
************************************************************
 AUTHEDF-WRITE-SEC      SECTION.
*
     MOVE     SPACE          TO   AUH-REC.
     INITIALIZE                   AUH-REC.
     MOVE     50             TO   AUH-F01.
     MOVE     WK-HACNO       TO   AUH-F02.
     MOVE     ZERO           TO   AUH-F03.
     MOVE     ZERO           TO   AUH-F04.
     MOVE     AUT-F14        TO   AUH-F05.
     MOVE     AUT-F02        TO   AUH-F06.
     MOVE     ZERO           TO   AUH-F07.
     MOVE     ZERO           TO   AUH-F08.
     MOVE     ZERO           TO   AUH-F09.
     MOVE     ZERO           TO   AUH-F10.
     MOVE     ZERO           TO   AUH-F11.
     MOVE     ZERO           TO   AUH-F13.
     MOVE     ZERO           TO   AUH-F141.
     MOVE     ZERO           TO   AUH-F142.
     MOVE     SPACE          TO   AUH-F15.
     MOVE     ZERO           TO   AUH-F16.
     MOVE     AUT-F01        TO   AUH-F17.
     MOVE     SPACE          TO   AUH-F18.
     MOVE     SPACE          TO   AUH-F19.
     MOVE     ZERO           TO   AUH-F20.
     MOVE     ZERO           TO   AUH-F21.
     MOVE     ZERO           TO   AUH-F22.
     MOVE     1              TO   AUH-F23.
     MOVE     ZERO           TO   AUH-F24.
     MOVE     1              TO   AUH-F25.
     MOVE     ZERO           TO   AUH-F33.
     MOVE     ZERO           TO   AUH-F34.
     MOVE     SYS-DATE       TO   AUH-F98.
     MOVE     SYS-DATE       TO   AUH-F99.
     WRITE    AUH-REC.
     ADD      1              TO   CNT-AUH.
*
 AUTHEDF-WRITE-EXIT.
     EXIT.
************************************************************
*      2.3.2.3    自動発注明細ワーク出力                   *
************************************************************
 AUTMEIF-WRITE-SEC      SECTION.
*
     MOVE     SPACE          TO   AUM-REC.
     INITIALIZE                   AUM-REC.
     MOVE     50             TO   AUM-F01.
     MOVE     WK-HACNO       TO   AUM-F02.
     MOVE     IXA            TO   AUM-F03.
     MOVE     ZERO           TO   AUM-F04.
     MOVE     ZERO           TO   AUM-F05.
     MOVE     AUT-F031       TO   AUM-F06.
     MOVE     AUT-F032       TO   AUM-F07.
     MOVE     AUT-F04        TO   AUM-F08.
     MOVE     AUT-F10        TO   AUM-F09.
     MOVE     ZERO           TO   AUM-F10.
     IF       AUT-F11   NOT =     ZERO
              MOVE      SPACE     TO   AUM-F11
     ELSE
              MOVE     "4"        TO   AUM-F11
     END-IF.
     MOVE     AUT-F11        TO   AUM-F12.
     MOVE     SPACE          TO   AUM-F13.
     MOVE     AUT-F12        TO   AUM-F14.
     MOVE     AUT-F13        TO   AUM-F15.
     MOVE     SPACE          TO   AUM-F16.
     MOVE     ZERO           TO   AUM-F17.
     MOVE     SPACE          TO   AUM-F18.
     MOVE     ZERO           TO   AUM-F19.
     MOVE     AUT-F01        TO   AUM-F97.
     MOVE     SYS-DATE       TO   AUM-F98.
     MOVE     SYS-DATE       TO   AUM-F99.
     WRITE    AUM-REC.
     ADD      1              TO   CNT-AUM.
*
 AUTMEIF-WRITE-EXIT.
     EXIT.
*****************<<  PROGRAM  END  >>***********************

```
