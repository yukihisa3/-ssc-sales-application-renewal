****************************************************************
*                                                              *
*　　顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*　　業務名　　　　　　　：　出荷検品システム                  *
*　　モジュール名　　　　：　検品明細Ｆ更新　　　　　　　　　　*
*　　作成日／更新日　　　：　00/11/08                          *
*　　作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*　　処理概要　　　　　　：　検品明細Ｆ・在庫マスタ　　　　　　*
*　　　　　　　　　　　　　　売上伝票Ｆ　を更新する　　　　　  *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SKEDELEB.
 AUTHOR.                N.KANEKO.
 DATE-WRITTEN.          00/11/08.
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SPECIAL-NAMES.
         CONSOLE   IS   CONS
         YA        IS   CHR-2
         YB-21     IS   CHR-21
         YB        IS   CHR-15.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 検品明細Ｆ >>--*
     SELECT   RCVSYUF   ASSIGN         DA-01-S-RCVSYUF
                        ORGANIZATION   SEQUENTIAL
                        STATUS         RCV-ST.
*----<< 累積検品明細Ｆ >>--*
     SELECT   RUISYUF   ASSIGN         DA-01-VI-RUISYUF
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  RUI-F04 RUI-F06 RUI-F07
                        STATUS         RUI-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 検品明細Ｆ >>--*
 FD  RCVSYUF            LABEL RECORD   IS   STANDARD
     BLOCK              CONTAINS       42   RECORDS.
     COPY        RCVSYUF     OF      XFDLIB
                 JOINING     RCV     PREFIX.
*----<< 累積検品明細Ｆ >>--*
 FD  RUISYUF            LABEL RECORD   IS   STANDARD
     BLOCK              CONTAINS       1    RECORDS.
     COPY        RUISYUF     OF      XFDLIB
                 JOINING     RUI     PREFIX.
*
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  END-FLG        PIC  9(01).
     03  CHK-FLG        PIC  9(01).
     03  HMEIMS-INV-FLG PIC  X(03)  VALUE  SPACE.
 01  COUNTERS.
     03  IN-CNT         PIC  9(06).
     03  OUT-CNT        PIC  9(06).
*
*----<< ｴﾗｰﾒｯｾｰｼﾞ >>--*
 01  ERR-TBL.
   03    FILLER         PIC  N(14)     VALUE
                        NC"検品明細ファイル　重複".
   03    FILLER         PIC  N(14)     VALUE
                        NC"売上伝票ファイル未登録　　　".
   03    FILLER         PIC  N(14)     VALUE
                        NC"商品変換テーブル未登録　　　".
   03    FILLER         PIC  N(14)     VALUE
                        NC"在庫マスタ未登録　　　　　　".
   03    FILLER         PIC  N(14)     VALUE
                        NC"商品名称マスタ未登録　　　　".
   03    FILLER         PIC  N(14)     VALUE
                        NC"出荷数量＞発注数量　エラー　".
   03    FILLER         PIC  N(14)     VALUE
                        NC"出荷数量が０以下です。確認！".
 01  ERR-TBL-R          REDEFINES  ERR-TBL.
   03  ERR-MSG          PIC  N(14)     OCCURS   7.
*
*----<< ﾜｰｸ ｴﾘｱ >>--*
 01  WK-GOKEI           PIC  9(10)     VALUE  ZERO.
 01  WK-DEN-F27D        PIC  9(01)     VALUE  ZERO.
 01  ID                 PIC  9(01)     VALUE  ZERO.
 01  WK-DATE            PIC  9(08).
 01  WK-DATE-R          REDEFINES      WK-DATE.
     03  WK-DATE-R1     PIC  X(04).
     03  WK-DATE-R2     PIC  X(02).
     03  WK-DATE-R3     PIC  X(02).
 01  WK-RCV-F10         PIC  S9(9)V99  VALUE  ZERO.
 01  WK-RCV-F12         PIC  S9(9)V99  VALUE  ZERO.
 01  WK-SURYO           PIC  S9(9)V99  VALUE  ZERO.
 01  P-CNT              PIC  9(03)     VALUE  ZERO.
 01  L-CNT              PIC  9(02)     VALUE  99.
 01  HEN-DATE           PIC  9(08)     VALUE ZERO.
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  RCV-ST             PIC  X(02).
 01  RUI-ST             PIC  X(02).
 01  ZAI-ST             PIC  X(02).
 01  DEN-ST             PIC  X(02).
 01  SHO-ST             PIC  X(02).
 01  PRT-ST             PIC  X(02).
 01  ERR-ST             PIC  X(02).
 01  MEI-ST             PIC  X(02).
*
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
 01  SYS-YYMD           PIC  9(08).
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
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*
****************************************************************
 PROCEDURE              DIVISION.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 検品明細Ｆ >>--*
 RCV-ERR              SECTION.
     USE AFTER     EXCEPTION PROCEDURE      RCVSYUF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     "4000"         TO   PROGRAM-STATUS.
     DISPLAY  "### SKEDELEB RCVSYUF    ERROR " RCV-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 累計出荷ファイル >>--*
 RUI-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      RUISYUF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     "4000"         TO   PROGRAM-STATUS.
     DISPLAY  "### SKEDELEB RUISYUF    ERROR " RUI-ST " "
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
     PERFORM  MAIN-RTN   UNTIL     END-FLG   =    1.
     PERFORM  END-RTN.
     STOP RUN.
 PROG-CNTL-EXIT.
     EXIT.
****************************************************************
*　　　　初期処理　　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-RTN               SECTION.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SKEDELEB START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     SYS-DATE            TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   HEN-DATE.
     OPEN     INPUT     RCVSYUF.
     OPEN     I-O       RUISYUF.
*ワークエリア　クリア
     INITIALIZE         COUNTERS.
     INITIALIZE         FLAGS.
*
     PERFORM  210-READ-SEC.
*
 INIT-RTN-EXIT.
     EXIT.
****************************************************************
*　　　　メイン処理　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-RTN               SECTION.
*
*累積データを参照
     MOVE     RCV-F04   TO   RUI-F04.
     MOVE     RCV-F06   TO   RUI-F06.
     MOVE     RCV-F07   TO   RUI-F07.
     PERFORM  220-READ-SEC.
*
     IF  CHK-FLG   =    0
         CONTINUE
     ELSE
         DELETE  RUISYUF
         ADD     1          TO   OUT-CNT
     END-IF.
*
 MAIN-01.
*
     PERFORM  210-READ-SEC.
*
 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　エンド処理　　　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-RTN                SECTION.
*
     CLOSE    RCVSYUF
              RUISYUF.
*
     DISPLAY  "+++ INPUT      =" IN-CNT  " +++" UPON CONS.
     DISPLAY  "+++ DELETE     =" OUT-CNT " +++" UPON CONS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SKEDELEB END   *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
*
 END-RTN-EXIT.
     EXIT.
****************************************************************
*　　　　インプットデータ読む　　　　　　　　　　　　　　　　　*
****************************************************************
 210-READ-SEC           SECTION.
*
     READ     RCVSYUF
        AT    END
              MOVE      1    TO   END-FLG
              GO   TO   210-READ-EXIT
        NOT AT END
              ADD       1    TO   IN-CNT
     END-READ.
*
 210-READ-EXIT.
     EXIT.
****************************************************************
*　　　　アウトプットデータ読む　　　　　　　　　　　　　　　　*
****************************************************************
 220-READ-SEC           SECTION.
*
     READ     RUISYUF
        INVALID
              MOVE     0     TO   CHK-FLG
        NOT INVALID
              MOVE     1     TO   CHK-FLG
     END-READ.
*
 220-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
