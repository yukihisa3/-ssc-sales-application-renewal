****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　受領返品取込機能構築　　　　      *
*    業務名　　　　　　　：　返品伝票計上ＤＴリカバリ          *
*    モジュール名　　　　：　返品伝票計上ＤＴ検収日、入力日変更*
*    作成日／更新日　　　：　2020/08/18                        *
*    作成者／更新者　　　：　NAV T.TAKAHASHI                   *
*    処理概要　　　　　　：　指定されて範囲データの検収日、入力*
*                            日を変更する。　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SJR9902B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2020/08/18.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*受領返品累積ファイル
     SELECT   COMRHEF   ASSIGN    TO        DA-01-VI-COMRHEL3
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       RHE-F80  RHE-F82
                                            RHE-F83  RHE-F01
                                            RHE-F02  RHE-F07
                                            RHE-F04  RHE-F05
                                            WITH  DUPLICATES
                        FILE  STATUS   IS   RHE-STATUS.
*
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    受領返品累積ファイル
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
 01  DEN-ADD-CNT             PIC  9(07)     VALUE  ZERO.
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
*メッセージ定義エリア
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SJR99902".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SJR99902".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SJR99902".
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
*売上伝票ファイル内容格納
     COPY   SHTDENF  OF XFDLIB  JOINING   DWK  AS   PREFIX.
*
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE             DIVISION.
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   COMRHEF.
     MOVE      "COMRHEL4"   TO   AB-FILE.
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
*受領返品累積ファイルスタート
     PERFORM  COMRHEF-START-SEC.
*
 INIT-EXIT.
     EXIT.
*
****************************************************************
*　　受領返品累積ファイルスタート
****************************************************************
 COMRHEF-START-SEC     SECTION.
*
     MOVE    "COMRHEF-START-SEC" TO   S-NAME.
*スタートの為、レコード初期化
     MOVE     SPACE              TO   RHE-REC.
     INITIALIZE                       RHE-REC.
*スタートキーセット
     MOVE     SPACE              TO   RHE-REC.
     INITIALIZE                       RHE-REC.
     MOVE     "1"                TO   RHE-F80.
     MOVE     "D2"               TO   RHE-F82.
*
     START  COMRHEF  KEY  IS  >=  RHE-F80  RHE-F82  RHE-F83
                                  RHE-F01  RHE-F02  RHE-F07
                                  RHE-F04  RHE-F05
           INVALID
           MOVE  "END"           TO      END-FLG
           DISPLAY "＃処理対象がありません１＃" UPON CONS
           GO                    TO      COMRHEF-START-EXIT
     END-START.
*取込数量確定ファイル読込
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
*入力日変更
     MOVE     SYS-DATEW          TO   RHE-F83.
     MOVE     20200806           TO   RHE-F07.
     REWRITE  RHE-REC.
*
     PERFORM  COMRHEF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　受領返品累積ファイル読込                                  *
****************************************************************
 COMRHEF-READ-SEC            SECTION.
*
     MOVE    "COMRHEF-READ-SEC" TO        S-NAME.
*
     READ  COMRHEF  AT  END
           MOVE  "END"          TO        END-FLG
           GO                   TO        COMRHEF-READ-EXIT
     END-READ.
*件数カウント
     ADD      1                 TO        IN-CNT.
*処理経過出力
     IF  IN-CNT(5:3)  =  "000" OR "500"
         DISPLAY "## READ-CNT = " IN-CNT " ##" UPON CONS
     END-IF.
 READ010.
*計上区分が１より大きい場合は終了へ
     IF  RHE-F80  >  "1"
         MOVE     "END"         TO        END-FLG
         GO                     TO        COMRHEF-READ-EXIT
     END-IF.
 READ020.
*担当者がＤ２以外は終了へ
     IF  RHE-F82  NOT =  "D2"
         MOVE     "END"         TO        END-FLG
         GO                     TO        COMRHEF-READ-EXIT
     END-IF.
 READ030.
*取引先ＣＤチェック
     IF  RHE-F01  =  321717
         CONTINUE
     ELSE
         GO                     TO        COMRHEF-READ-SEC
     END-IF.
 READ040.
*検収日チェック
     IF  RHE-F07  >= 20200701
         CONTINUE
     ELSE
         GO                     TO        COMRHEF-READ-SEC
     END-IF.
 READ050.
*計上日チェック（計上済のデータは対象外）
     IF  RHE-F86  >  ZERO
         GO                     TO        COMRHEF-READ-SEC
     END-IF.
*
     ADD 1                      TO        TAISYO-CNT.
*
 COMRHEF-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*件数印字
*読込カウント
     DISPLAY "## READ-CNT        = " IN-CNT           UPON CONS.
*対象カウント
     DISPLAY "## TAISYO-CNT      = " TAISYO-CNT       UPON CONS.
*
     CLOSE  COMRHEF.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
