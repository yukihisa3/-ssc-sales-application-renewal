****************************************************************
*　　顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*　　業務名　　　　　　　：　出荷検品システム                  *
*　　モジュール名　　　　：　検品結果送信前チェック処理　　　　*
*　　作成日／更新日　　　：　2009/02/19                        *
*　　作成者／更新者　　　：　NAV                               *
*　　処理概要　　　　　　：　累積検品ファイルより、送信前のデー*
*　　　　　　　　　　　　　　タで発注数を異なるデータを抽出する*
*　　更新日／更新者　　　：　2011/10/06 / YOSHIDA.M            *
*　　修正概要　　　　　　：　基幹サーバ統合                    *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SKE6000B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          09/02/19.
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SPECIAL-NAMES.
         CONSOLE   IS   CONS
         YA        IS   CHR-2
         YB-21     IS   CHR-21
         YB        IS   CHR-15.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*---<<  出荷情報データ  >>---*
     SELECT   RUISYUF   ASSIGN         DA-01-VI-RUISYUL2
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  RUI-F11
                                       RUI-F04
                                       RUI-F03
                                       RUI-F02
                                       RUI-F05
                                       RUI-F06
                                       RUI-F07
                        STATUS         RUI-ST.
*----<< 伝票データ >>--*
     SELECT   SHTDENF   ASSIGN    TO   DA-01-VI-SHTDENL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  DEN-F01
                                       DEN-F02
                                       DEN-F04
                                       DEN-F051
***2011.10.06(DEN-F07,DEN-F112)
                                       DEN-F07
                                       DEN-F112
                                       DEN-F03
                        FILE  STATUS   DEN-ST.
*伝票データワーク(商品単位順）
     SELECT  CHKFILL3  ASSIGN    TO    DA-01-VI-CHKFILL3
                       ORGANIZATION    INDEXED
                       ACCESS    MODE  RANDOM
                       RECORD    KEY   KP3-F46
                                       KP3-F47
                                       KP3-F01
                                       KP3-F48
                                       KP3-F25
                                       KP3-F07
                                       KP3-F02
                                       KP3-F03
                       FILE  STATUS    KP3-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 出荷情報データ >>--*
 FD  RUISYUF            LABEL RECORD   IS   STANDARD.
     COPY        RUISYUF     OF      XFDLIB
                 JOINING     RUI     PREFIX.
*----<< 伝票データ >>--*
 FD  SHTDENF            LABEL     RECORD   IS   STANDARD.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   DEN       PREFIX.
*----<< 欠品ファイル >>--*
 FD  CHKFILL3
                       LABEL     RECORD    IS   STANDARD.
     COPY        CHKFILF     OF      XFDLIB
                 JOINING     KP3     PREFIX.
*
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  END-FLG         PIC  X(03)     VALUE  SPACE.
     03  END-FLG2        PIC  X(03)     VALUE  SPACE.
     03  SHTDENF-INV-FLG PIC  X(03)     VALUE  SPACE.
 01  COUNTERS.
     03  IN-CNT          PIC  9(06).
     03  OUT-CNT         PIC  9(06).
*
*----<< ﾜｰｸ ｴﾘｱ >>--*
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  RUI-ST             PIC  X(02).
 01  DEN-ST             PIC  X(02).
 01  KP3-ST             PIC  X(02).
*
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
 01  WK-SYS-DATE        PIC  9(08).
 01  WK-SYS-TIME        PIC  9(06).
 01  SYS-DATE           PIC  9(06).
 01  FILLER             REDEFINES      SYS-DATE.
     03  SYS-YY         PIC  9(02).
     03  SYS-MM         PIC  9(02).
     03  SYS-DD         PIC  9(02).
 01  SYS-TIME           PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-HH         PIC  9(02).
     03  SYS-MN         PIC  9(02).
     03  SYS-SS         PIC  9(02).
     03  SYS-MS         PIC  9(02).
 01  LINK-AREA2.
     03  LINK-IN-KBN    PIC  X(01).
     03  LINK-IN-YMD6   PIC  9(06).
     03  LINK-IN-YMD8   PIC  9(08).
     03  LINK-OUT-RET   PIC  X(01).
     03  LINK-OUT-YMD8  PIC  9(08).
*
****************************************************************
 PROCEDURE              DIVISION.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 出荷情報データ >>--*
 RUI-ERR              SECTION.
     USE AFTER     EXCEPTION PROCEDURE      RUISYUF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     "4000"         TO   PROGRAM-STATUS.
     DISPLAY  "### SKE6000B RUISYUF    ERROR " RUI-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 伝票データ >>--*
 DENL-ERR               SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SHTDENF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     "4000"         TO   PROGRAM-STATUS.
     DISPLAY  "### SKE6000B SHTDENL1   ERROR " DEN-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< ケーヨー送信データ >>--*
 KP3-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      CHKFILL3.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     "4000"         TO   PROGRAM-STATUS.
     DISPLAY  "### SKE6000B CHKFILL3 ERROR " KP3-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 END DECLARATIVES.
****************************************************************
*　　　　メインモジュール　　　　　　　　　　　　　　　　　　　*
****************************************************************
 PROG-CNTL              SECTION.
     PERFORM  INIT-RTN.
     PERFORM  MAIN-RTN   UNTIL     END-FLG   =   "END".
     PERFORM  END-RTN.
     STOP RUN.
 PROG-CNTL-EXIT.
     EXIT.
****************************************************************
*　　　　初期処理　　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-RTN               SECTION.
     ACCEPT      SYS-DATE    FROM  DATE.
     ACCEPT      SYS-TIME    FROM  TIME.
*    システム日付８桁変換
     MOVE    "3"        TO        LINK-IN-KBN.
     MOVE     SYS-DATE  TO        LINK-IN-YMD6.
     CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD8.
     MOVE     LINK-OUT-YMD8  TO   WK-SYS-DATE.
     MOVE     SYS-TIME(1:6)  TO   WK-SYS-TIME.
*ファイルＯＰＥＮ
     OPEN     INPUT     RUISYUF SHTDENF.
     OPEN     I-O       CHKFILL3.
*ワークエリア　クリア
     INITIALIZE         COUNTERS.
     INITIALIZE         FLAGS.
*出荷情報データ初期読込み
     PERFORM  RUISYUF-READ-SEC.
     IF   END-FLG  =  "END"
          DISPLAY NC"＃＃対象データ無し＃＃" UPON CONS
     END-IF.
*
 INIT-RTN-EXIT.
     EXIT.
****************************************************************
*　　　　メイン処理　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-RTN               SECTION.
*    売上伝票ファイル読み込み
     PERFORM  SHTDENF-READ-SEC.
*
     IF       SHTDENF-INV-FLG  =  SPACE
              MOVE     DEN-REC   TO    KP3-REC
              MOVE     RUI-F10   TO    KP3-F15
              READ     CHKFILL3
                       INVALID  KEY
                       WRITE     KP3-REC
              END-READ
*
              ADD      1              TO   OUT-CNT
     END-IF.
*    出荷情報読込み
     PERFORM  RUISYUF-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　エンド処理　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-RTN                SECTION.
*
     CLOSE    RUISYUF  SHTDENF  CHKFILL3.
*
     DISPLAY  "## INPUT  CNT = " IN-CNT    UPON CONS.
     DISPLAY  "## OUTPUT CNT = " OUT-CNT   UPON CONS.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SKE6000B END   *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
*
 END-RTN-EXIT.
     EXIT.
****************************************************************
*　　出荷情報データ読込み　　　　　　　　　　　　　　　　　　　*
****************************************************************
 RUISYUF-READ-SEC       SECTION.
*    売上伝票ファイル参照
     READ     RUISYUF   AT  END
              MOVE  "END"    TO   END-FLG
              GO             TO   RUISYUF-READ-EXIT
     END-READ.
*    読込みカウント
     ADD      1              TO   IN-CNT.
*    送信ＦＬＧチェック
     IF       RUI-F11  =  1
              MOVE  "END"    TO   END-FLG
              GO             TO   RUISYUF-READ-EXIT
     END-IF.
*    発注数と出荷数の比較
     IF       RUI-F09  =  RUI-F10
              GO             TO   RUISYUF-READ-SEC
     END-IF.
*
     ADD      1              TO   IN-CNT.
*
 RUISYUF-READ-EXIT.
     EXIT.
****************************************************************
*　　売上伝票Ｆ　送信済ＦＬＧ更新　　　　　　　　　　　　　　　*
****************************************************************
 SHTDENF-READ-SEC       SECTION.
*    売上伝票Ｆ読み込み
     MOVE     RUI-F01        TO   DEN-F01.
     MOVE     RUI-F06        TO   DEN-F02.
     MOVE     ZERO           TO   DEN-F04.
     MOVE     40             TO   DEN-F051.
     MOVE     RUI-F07        TO   DEN-F03.
***2011.10.06 ST
     MOVE     RUI-F02        TO   DEN-F07.
     MOVE     RUI-F05        TO   DEN-F112.
***2011.10.06 EN
     READ     SHTDENF   INVALID
              MOVE   "INV"   TO   SHTDENF-INV-FLG
              NOT  INVALID
              MOVE   SPACE   TO   SHTDENF-INV-FLG
     END-READ.
*
 SHTDENF-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
