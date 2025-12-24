****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　受領返品　　　　　　　            *
*    サブシステム　　　　：　受領返品データ削除　　　　　      *
*    作成日／作成者　　　：　2023/01/06  NAV TAKAHASHI         *
*    処理概要　　　　　　：　受領返品データの削除を行なう。　　*
*    更新日／更新者　　　：　                                  *
*    更新概要　　　　　　：　　　　　　　　　　　　　　　　　　*
*    　　　　　　　　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SJR9999B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2021/11/10.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*返品累積ファイル
     SELECT   COMRHEF   ASSIGN    TO        DA-01-VI-COMRHEL2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       RHE-F78  RHE-F79
                                            RHE-F01  RHE-F02
                                            RHE-F03  RHE-F04
                                            RHE-F05
                        FILE  STATUS   IS   RHE-STATUS.
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    返品累積ファイル
******************************************************************
 FD  COMRHEF            LABEL RECORD   IS   STANDARD.
     COPY     COMRHEF   OF        XFDLIB
              JOINING   RHE       PREFIX.
*
******************************************************************
 WORKING-STORAGE        SECTION.
******************************************************************
*FLG/ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  ZERO.
 01  IN-CNT                  PIC  9(07)     VALUE  ZERO.
 01  RWK-ADD-CNT             PIC  9(07)     VALUE  ZERO.
 01  TAISYO-CNT              PIC  9(07)     VALUE  ZERO.
 01  SHTDENF-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  HSHOTBL-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  SHOTBL2-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  HMEIMS-INV-FLG          PIC  X(03)     VALUE  SPACE.
 01  HTOKMS-INV-FLG          PIC  X(03)     VALUE  SPACE.
 01  HTENMS-INV-FLG          PIC  X(03)     VALUE  SPACE.
 01  HJYOKEN-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  KEIJYOF-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  WK-TANABAN              PIC  X(06)     VALUE  SPACE.
 01  WK-RHE-F16              PIC  9(05)V9(01) VALUE  ZERO.
*システム日付の編集
 01  WK-DTTM-AREA.
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
     03  SYS-TIME.
         05  WK-SYS-TIME1  PIC 9(06).
         05  WK-SYS-TIME2  PIC 9(02).
*ファイルＳＴＡＴＵＳエリア
 01  WK-ST.
     03  RHE-STATUS        PIC  X(02).
     03  RWK-STATUS        PIC  X(02).
*メッセージ定義エリア
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SJR9999B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SJR9999B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SJR9999B".
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
*在庫締め日退避ワーク
 01  WK-ZAIKO-SIME          PIC   9(06)  VALUE  ZERO.
 01  WK-SIMEBI.
     03  WK-SIMEBI-YY       PIC   9(04).
     03  WK-SIMEBI-MM       PIC   9(02).
*数量変換ワーク
 01  WK-SURYO.
     03  WK-SURYO-HEN       PIC   9(05)V9(01)  VALUE  ZERO.
*伝票備考ワーク
 01  WK-BIKO.
     03  WK-BIKO-1          PIC   X(15)  VALUE  SPACE.
     03  WK-BIKO-2          PIC   X(15)  VALUE  SPACE.
*ブレイクキー
 01  WK-KEY.
*#2017/12/04 NAV ST
     03  WK-RHE-F01         PIC   9(08)  VALUE  ZERO.
*#2017/12/04 NAV ED
     03  WK-RHE-F02         PIC   9(05)  VALUE  ZERO.
     03  WK-RHE-F07         PIC   9(08)  VALUE  ZERO.
     03  WK-RHE-F04         PIC   9(09)  VALUE  ZERO.
*日付サブルーチン用
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*2020/05/25 NAV ST Ｄ３６５伝票番号取得
 01  WK-D365-RWK-PARA.
     03  WK-D365-PARA-IN1   PIC   X(01).
     03  WK-D365-PARA-IN2   PIC   9(08).
     03  WK-D365-PARA-OUT1  PIC   X(20).
     03  WK-D365-PARA-OUT2  PIC   X(01).
*
*
 LINKAGE              SECTION.,
 01  PARA-HIDUKE            PIC   9(08).
 01  PARA-JIKAN             PIC   9(04).
 01  PARA-TOKCD             PIC   9(08).
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE             DIVISION  USING
                       PARA-HIDUKE  PARA-JIKAN  PARA-TOKCD.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   COMRHEF.
     MOVE      "COMRHELA"   TO   AB-FILE.
     MOVE      RHE-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 END     DECLARATIVES.
*****************************************************************
*                                                                *
******************************************************************
 GENERAL-PROCESS       SECTION.
*
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC   UNTIL  END-FLG = "END".
     PERFORM  END-SEC.
*
     STOP  RUN.
*
 GENERAL-EXIT.
     EXIT.
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
*ファイルＯＰＥＮ
     OPEN     I-O       COMRHEF.
*
     DISPLAY  MSG-START UPON CONS.
*システム時刻取得
     ACCEPT      SYS-TIME  FROM      TIME.
*システム日付取得
     ACCEPT      SYS-DATE  FROM      DATE.
*システム日付８桁変換
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
*返品累積ファイルスタート
     PERFORM  COMRHEF-START-SEC.
     IF       END-FLG  =   "END"
              GO       TO   INIT-EXIT
     END-IF.
*
 INIT-EXIT.
     EXIT.
*
****************************************************************
*　　返品累積ファイル初期ＲＥＡＤ
****************************************************************
 COMRHEF-START-SEC     SECTION.
*
     MOVE    "COMRHEF-START-SEC" TO   S-NAME.
*スタートの為、レコード初期化
     MOVE     SPACE              TO   RHE-REC.
     INITIALIZE                       RHE-REC.
*スタートキーセット
     MOVE     PARA-HIDUKE        TO   RHE-F78.
     MOVE     PARA-JIKAN         TO   RHE-F79.
     MOVE     PARA-TOKCD         TO   RHE-F01.
*
     START  COMRHEF  KEY  IS  >=  RHE-F78  RHE-F79  RHE-F01
                                  RHE-F02  RHE-F03  RHE-F04
                                  RHE-F05
           INVALID
           MOVE  "END"           TO      END-FLG
           DISPLAY "＃処理対象がありません１＃" UPON CONS
           GO                    TO      COMRHEF-START-EXIT
     END-START.
*返品累積ファイル読込
     PERFORM  COMRHEF-READ-SEC.
     IF   END-FLG = "END"
          DISPLAY "＃処理対象がありません２＃" UPON CONS
     END-IF.
*
 COMRHEF-START-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"          TO   S-NAME.
*データ削除
     DELETE   COMRHEF.
     ADD      1                  TO   TAISYO-CNT.
*
 MAIN-020.
     PERFORM  COMRHEF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
*
****************************************************************
*　　返品累積ファイル読込                                  *
****************************************************************
 COMRHEF-READ-SEC            SECTION.
*
     MOVE    "COMRHEF-READ-SEC" TO        S-NAME.
*
     READ  COMRHEF  AT  END
           MOVE  "END"          TO        END-FLG
           GO                   TO        COMRHEF-READ-EXIT
     END-READ.
*
     IF    PARA-HIDUKE  NOT =  RHE-F78
     OR    PARA-JIKAN   NOT =  RHE-F79
     OR    PARA-TOKCD   NOT =  RHE-F01
           MOVE  "END"          TO        END-FLG
           GO                   TO        COMRHEF-READ-EXIT
     END-IF.
*件数カウント
     ADD      1                 TO        IN-CNT.
     IF  IN-CNT(5:3) =  "000" OR "500"
         DISPLAY "READ-CNT = " IN-CNT UPON CONS
     END-IF.
*
 COMRHEF-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"          TO    S-NAME.
*読込カウント
     DISPLAY "## READ-CNT        = " IN-CNT           UPON CONS.
*対象カウント
     DISPLAY "## DELETE-CNT      = " TAISYO-CNT       UPON CONS.
*
     CLOSE  COMRHEF.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
