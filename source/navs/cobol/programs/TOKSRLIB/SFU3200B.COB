****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ＨＧ基幹システム　　　　　　　　　*
*    業務名　　　　　　　：　ＡＣＯＳ振替改善　　　　　　　　　*
*    モジュール名　　　　：　ＡＣＯＳ振替データ累積処理　　　　*
*    作成日／更新日　　　：　2016/12/26                        *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　ＡＣＯＳ振替データ更新ファイルの  *
*                            全レコードをＡＣＯＳ振替データ累　*
*                            積ファイルに累積する。            *
*　　更新日／更新者　　　：　                                  *
*　　更新日／更新者　　　：　                                  *
*    修正概要　　　　　　：                                    *
*      　　　　　　　　　　　　　　　　　　　　　　　　　　　　*
*      　　　　　　　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SFU3200B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          16/12/26.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*振替データ更新ファイル
     SELECT   FURIKAF   ASSIGN    TO        DA-01-VI-FURIKAL1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       FUR-F01  FUR-F02
                                            FUR-F03  FUR-F04
                                            FUR-F05
                        FILE  STATUS   IS   FUR-ST.
*ＡＣＯＳ振替データ累積ファイル
     SELECT   FRACOSF   ASSIGN    TO        DA-01-VI-FRACOSL2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       FRA-F01  FRA-F03
                                            FRA-F04  FRA-F08
                                            FRA-F09  FRA-F05
                                            FRA-F07  FRA-F06
                        FILE  STATUS   IS   FRA-ST.
*ＡＣＯＳ振替データ累積エラーファイル
     SELECT   FRACOEF   ASSIGN    TO        DA-01-VI-FRACOEL2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       FRE-F01  FRE-F03
                                            FRE-F04  FRE-F08
                                            FRE-F09  FRE-F05
                                            FRE-F07  FRE-F06
                        FILE  STATUS   IS   FRA-ST.
*********
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    振替データ更新ファイル
******************************************************************
 FD  FURIKAF
                        LABEL RECORD   IS   STANDARD.
     COPY     FURIKAF   OF        XFDLIB
              JOINING   FUR  AS   PREFIX.
*
******************************************************************
*    ＡＣＯＳ振替データ累積ファイル
******************************************************************
 FD  FRACOSF
                        LABEL RECORD   IS   STANDARD.
     COPY     FRACOSF   OF        XFDLIB
              JOINING   FRA  AS   PREFIX.
*
******************************************************************
*    ＡＣＯＳ振替データ累積エラーファイル
******************************************************************
 FD  FRACOEF
                        LABEL RECORD   IS   STANDARD.
     COPY     FRACOEF   OF        XFDLIB
              JOINING   FRE  AS   PREFIX.
*
******************************************************************
 WORKING-STORAGE        SECTION.
*ワーク項目
 01  END-FLG                 PIC  X(03)     VALUE  SPACE.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT                 PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT1                PIC  9(08)     VALUE  ZERO.
 01  INV-CNT                 PIC  9(08)     VALUE  ZERO.
 01  INV-CNT1                PIC  9(08)     VALUE  ZERO.
 01  FRACOSF-INV-FLG         PIC  X(03)     VALUE  SPACE.
 01  FRACOEF-INV-FLG         PIC  X(03)     VALUE  SPACE.
*プログラムＳＴＡＴＵＳ.
 01  WK-ST.
     03  FUR-ST        PIC  X(02).
     03  FRA-ST        PIC  X(02).
     03  FRE-ST        PIC  X(02).
***** システム日付ワーク
 01  SYSTEM-HIZUKE.
     03  SYSYMD              PIC   9(06)  VALUE  ZERO.
     03  SYS-DATEW           PIC   9(08)  VALUE  ZERO.
     03  SYS-DATE-R          REDEFINES SYS-DATEW.
         05  SYS-YY          PIC   9(04).
         05  SYS-MM          PIC   9(02).
         05  SYS-DD          PIC   9(02).
***** システム時刻ワーク
 01  SYS-TIME           PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-HHMMSS     PIC  9(06).
     03  SYS-MS         PIC  9(02).
***  セクション名
 01  SEC-NAME.
     03  FILLER             PIC  X(05)     VALUE " *** ".
     03  S-NAME             PIC  X(30).
*メッセージ出力
 01  FILE-ERR.
     03  FRA-ERR           PIC  N(20)  VALUE
         NC"ＡＣＯＳ振替データ累積ファイルエラー".
     03  FUR-ERR           PIC  N(20)  VALUE
         NC"振替累積ファイルエラー".
     03  FRE-ERR           PIC  N(20)  VALUE
         NC"ＡＣＯＳ振替データ累積エラーＦエラー".
*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SFU3200B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SFU3200B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-IN.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(10)  VALUE " INPUT  = ".
         05  IN-CNT         PIC   9(08).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  MSG-OUT.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(10)  VALUE " OUTPUT = ".
         05  OUT-CNT        PIC   9(08).
         05  FILLER         PIC   X(05)  VALUE " *** ".
*    日付変換ワーク（パラメタ用）
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*パラメタ定義
 LINKAGE                SECTION.
 01  PARA-BUMON             PIC   X(04).
 01  PARA-TANCD             PIC   X(02).
 01  PARA-JDATE             PIC   9(08).
 01  PARA-JTIME             PIC   9(06).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE DIVISION  USING  PARA-BUMON  PARA-TANCD  PARA-JDATE
                            PARA-JTIME.
 DECLARATIVES.
 FUR-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE FURIKAF.
     DISPLAY     FUR-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     FUR-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
*
 FRA-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE FRACOSF.
     DISPLAY     FRA-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     FRA-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
*
 FRE-ERR                   SECTION.
     USE         AFTER     EXCEPTION PROCEDURE FRACOEF.
     DISPLAY     FRE-ERR   UPON      CONS.
     DISPLAY     SEC-NAME  UPON      CONS.
     DISPLAY     FRE-ST    UPON      CONS.
     MOVE        "4000"    TO        PROGRAM-STATUS.
     STOP        RUN.
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
              UNTIL     END-FLG    =   "END".
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
*
     OPEN     INPUT     FURIKAF.
     OPEN     I-O       FRACOSF   FRACOEF.
     DISPLAY  MSG-START UPON CONS.
*システム時刻取得
     ACCEPT   SYS-TIME  FROM      TIME.
*システム日付取得
     ACCEPT   SYSYMD    FROM      DATE.
     MOVE    "3"        TO        LINK-IN-KBN.
     MOVE     SYSYMD    TO        LINK-IN-YMD6.
     CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD8.
     IF       LINK-OUT-RET   =    ZERO
              MOVE      LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
              MOVE    ZERO             TO   SYS-DATEW
     END-IF.
*
     MOVE     SPACE     TO        END-FLG.
     MOVE     ZERO      TO        RD-CNT  INV-CNT  WRT-CNT.
*
 INIT-01.
*振替データ更新ファイル読込
     PERFORM  FURIKAF-READ-SEC.
     IF  END-FLG = "END"
         DISPLAY NC"＃累積対象ＤＴがありません！＃" UPON CONS
         GO             TO   INIT-EXIT
     END-IF.
*
 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　ＡＣＯＳ振替累積データ更新　　　　　　　　　　　*
****************************************************************
 FRACOSF-WT-SEC       SECTION.
*
     MOVE    "FRACOSF-WT-SEC"     TO   S-NAME.
*レコード初期化
     MOVE     SPACE               TO   FRA-REC.
     INITIALIZE                        FRA-REC.
*振替受信日
     MOVE     PARA-JDATE          TO   FRA-F01.
*振替受信時刻
     MOVE     PARA-JTIME          TO   FRA-F02.
*場所ＣＤ
     MOVE     FUR-F17             TO   FRA-F03.
*伝票区分
     MOVE     FUR-F02             TO   FRA-F04.
*伝票番号
     MOVE     FUR-F03             TO   FRA-F05.
*行番号
     MOVE     FUR-F04             TO   FRA-F06.
*赤黒区分
     MOVE     FUR-F05             TO   FRA-F07.
*出荷日
     MOVE     FUR-F09             TO   FRA-F08.
*サカタ商品ＣＤ
     MOVE     FUR-F10             TO   FRA-F09.
*ＡＣＯＳ振替データ累積ファイル存在チェック
     MOVE     SPACE               TO   FRACOSF-INV-FLG.
     PERFORM  FRACOSF-READ-SEC.
     IF  FRACOSF-INV-FLG  =  SPACE
         ADD  1                   TO   INV-CNT
         GO                       TO   FRACOSF-WT-EXIT
     END-IF.
*ＡＣＯＳ振替データ更新ファイルレコードセット
     MOVE     FUR-REC             TO   FRA-F10.
*データ作成日
     MOVE     SYS-DATEW           TO   FRA-F96.
*データ作成時刻
     MOVE     SYS-HHMMSS          TO   FRA-F97.
*データ作成担当者部門ＣＤ
     MOVE     PARA-BUMON          TO   FRA-F98.
*データ作成担当者ＣＤ
     MOVE     PARA-TANCD          TO   FRA-F99.
*振替累積ファイル出力
     WRITE    FRA-REC.
*件数カウント
     ADD      1                   TO   WRT-CNT.
*
 FRACOSF-WT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　ＡＣＯＳ振替累積エラー更新　　　　　　　　　　　*
****************************************************************
 FRACOEF-WT-SEC       SECTION.
*
     MOVE    "FRACOEF-WT-SEC"     TO   S-NAME.
*
*レコード初期化
     MOVE     SPACE               TO   FRE-REC.
     INITIALIZE                        FRE-REC.
*振替受信日
     MOVE     PARA-JDATE          TO   FRE-F01.
*振替受信時刻
     MOVE     PARA-JTIME          TO   FRE-F02.
*場所ＣＤ
     MOVE     FUR-F17             TO   FRE-F03.
*伝票区分
     MOVE     FUR-F02             TO   FRE-F04.
*伝票番号
     MOVE     FUR-F03             TO   FRE-F05.
*行番号
     MOVE     FUR-F04             TO   FRE-F06.
*赤黒区分
     MOVE     FUR-F05             TO   FRE-F07.
*出荷日
     MOVE     FUR-F09             TO   FRE-F08.
*サカタ商品ＣＤ
     MOVE     FUR-F10             TO   FRE-F09.
*ＡＣＯＳ振替データ累積ファイル存在チェック
     MOVE     SPACE               TO   FRACOEF-INV-FLG.
     PERFORM  FRACOEF-READ-SEC.
     IF  FRACOEF-INV-FLG  =  SPACE
         ADD  1                   TO   INV-CNT1
         GO                       TO   FRACOEF-WT-EXIT
     END-IF.
*ＡＣＯＳ振替データ更新ファイルレコードセット
     MOVE     FUR-REC             TO   FRE-F10.
*データ作成日
     MOVE     SYS-DATEW           TO   FRE-F96.
*データ作成時刻
     MOVE     SYS-HHMMSS          TO   FRE-F97.
*データ作成担当者部門ＣＤ
     MOVE     PARA-BUMON          TO   FRE-F98.
*データ作成担当者ＣＤ
     MOVE     PARA-TANCD          TO   FRE-F99.
*振替累積ファイル出力
     WRITE    FRE-REC.
*件数カウント
     ADD      1                   TO   WRT-CNT1.
*
 FRACOEF-WT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*更新判定
     IF       FUR-F29  =  "1"
              PERFORM  FRACOEF-WT-SEC
     ELSE
              PERFORM  FRACOSF-WT-SEC
     END-IF.
 MAIN-010.
*振替データ更新ファイル読込
     PERFORM  FURIKAF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*プログラム終了メッセージ表示
     DISPLAY "READ-CNT = " RD-CNT      UPON  CONS.
     DISPLAY "ERR-CNT  = " INV-CNT     UPON  CONS.
     DISPLAY "WRT-CNT  = " WRT-CNT     UPON  CONS.
*ファイルクローズ
     CLOSE     FURIKAF FRACOSF FRACOEF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
****************************************************************
*　　　　　　振替データ更新ファイル読み込み
****************************************************************
 FURIKAF-READ-SEC          SECTION.
*
     READ     FURIKAF
              AT  END       MOVE  "END"   TO  END-FLG
                            GO TO FURIKAF-READ-EXIT
              NOT AT  END   ADD    1      TO  RD-CNT
     END-READ.
*件数表示
     IF       RD-CNT(6:3)  =  "000" OR "500"
              DISPLAY "# READ-CNT = " RD-CNT  " #"
                      UPON CONS
     END-IF.
*更新区分チェック（１の場合はエラーの為、更新しない）
**   IF       FUR-F29  =  "1"
**            GO                          TO  FURIKAF-READ-SEC
**   END-IF.
*
 FURIKAF-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　ＡＣＯＳ振替データ累積ファイル読込
****************************************************************
 FRACOSF-READ-SEC          SECTION.
*
     READ     FRACOSF
              INVALID      MOVE  "INV"    TO  FRACOSF-INV-FLG
              NOT  INVALID MOVE  SPACE    TO  FRACOSF-INV-FLG
     END-READ.
*
 FRACOSF-READ-EXIT.
     EXIT.
****************************************************************
*　　　　　　ＡＣＯＳ振替データ累積エラーファイル読込
****************************************************************
 FRACOEF-READ-SEC          SECTION.
*
     READ     FRACOEF
              INVALID      MOVE  "INV"    TO  FRACOEF-INV-FLG
              NOT  INVALID MOVE  SPACE    TO  FRACOEF-INV-FLG
     END-READ.
*
 FRACOEF-READ-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*
