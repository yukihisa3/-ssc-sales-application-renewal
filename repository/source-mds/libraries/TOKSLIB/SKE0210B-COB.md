# SKE0210B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SKE0210B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*　　顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*　　業務名　　　　　　　：　出荷検品システム                  *
*　　モジュール名　　　　：　出荷ラベルヘッダＦ更新　　　　　　*
*　　作成日／更新日　　　：　00/11/08                          *
*　　作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*　　処理概要　　　　　　：　出荷ラベルヘッダＦを更新する      *
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SKE0210B.
 AUTHOR.                N.KANEKO.
 DATE-WRITTEN.          00/11/08.
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SPECIAL-NAMES.
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 出荷ラベルヘッダＦ >>--*
     SELECT   RCVFUTF   ASSIGN         DA-01-S-RCVFUTF
                        ORGANIZATION   SEQUENTIAL
                        STATUS         RCV-ST.
*----<< 累積出荷ラベルヘッダＦ >>--*
     SELECT   RUIFUTF   ASSIGN         DA-01-VI-RUIFUTF
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  RUI-F01 RUI-F02 RUI-F04
                        STATUS         RUI-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 出荷ラベルヘッダＦ >>--*
 FD  RCVFUTF            LABEL RECORD   IS   STANDARD
     BLOCK              CONTAINS       88   RECORDS.
     COPY        RCVFUTF     OF      XFDLIB
                 JOINING     RCV     PREFIX.
*----<< 累積出荷ラベルヘッダＦ >>--*
 FD  RUIFUTF            LABEL RECORD   IS   STANDARD
     BLOCK              CONTAINS       1    RECORDS.
     COPY        RUIFUTF     OF      XFDLIB
                 JOINING     RUI     PREFIX.
*
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  END-FLG        PIC  9(01).
     03  CHK-FLG        PIC  X(01).
 01  COUNTERS.
     03  IN-CNT         PIC  9(06).
     03  OUT-CNT        PIC  9(06).
*
*----<< ｴﾗｰﾒｯｾｰｼﾞ >>--*
 01  ERR-MSG            PIC  X(31)     VALUE
     "***  ｷｰｶﾞｼﾞｭｳﾌｸｼﾃｲﾏｽ  *** KEY= ".
*
*----<< ﾜｰｸ ｴﾘｱ >>--*
 01  WK-GOKEI           PIC  9(10)     VALUE  ZERO.
 01  ID                 PIC  9(01)     VALUE  ZERO.
 01  WK-DATE            PIC  9(08).
 01  WK-DATE-R          REDEFINES      WK-DATE.
     03  WK-DATE-R1     PIC  X(04).
     03  WK-DATE-R2     PIC  X(02).
     03  WK-DATE-R3     PIC  X(02).
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  RCV-ST             PIC  X(02).
 01  RUI-ST             PIC  X(02).
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
*
 LINKAGE                SECTION.
 01  PARA-INCNT         PIC  9(06).
 01  PARA-OUTCNT        PIC  9(06).
*
****************************************************************
 PROCEDURE              DIVISION  USING     PARA-INCNT
                                            PARA-OUTCNT.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 出荷ラベルヘッダＦ >>--*
 RUI-ERR              SECTION.
     USE AFTER     EXCEPTION PROCEDURE      RCVFUTF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     "4000"         TO   PROGRAM-STATUS.
     DISPLAY  "### SKE0210B RCVFUTF    ERROR " RCV-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 支払合計ファイル >>--*
 SHI-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      RUIFUTF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     "4000"         TO   PROGRAM-STATUS.
     DISPLAY  "### SKE0210B RUIFUTF ERROR " RUI-ST " "
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
     DISPLAY  "*** SKE0210B START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     INPUT     RCVFUTF.
     OPEN     I-O       RUIFUTF.
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
*キー項目転送
     MOVE     RCV-F01        TO   RUI-F01.
     MOVE     RCV-F02        TO   RUI-F02.
     MOVE     RCV-F04        TO   RUI-F04.
*    累積ファイル読む
     PERFORM  220-READ-SEC.
*
     EVALUATE CHK-FLG
         WHEN "O"
              DISPLAY  ERR-MSG RUI-F01 RUI-F02 RUI-F04
              UPON CONS
         WHEN "E"
              MOVE RCV-F01        TO   RUI-F01
              MOVE RCV-F02        TO   RUI-F02
              MOVE RCV-F04        TO   RUI-F04
              MOVE RCV-F03(1:4)   TO   WK-DATE-R1
              MOVE RCV-F03(6:2)   TO   WK-DATE-R2
              MOVE RCV-F03(9:2)   TO   WK-DATE-R3
              MOVE WK-DATE        TO   RUI-F03
              WRITE     RUI-REC
              ADD  1              TO   OUT-CNT
     END-EVALUATE.
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
     CLOSE    RCVFUTF.
     CLOSE    RUIFUTF.
*
     DISPLAY  "+++ INPUT      =" IN-CNT  " +++" UPON CONS.
     DISPLAY  "+++ OUTPUT     =" OUT-CNT " +++" UPON CONS.
     MOVE     IN-CNT         TO   PARA-INCNT.
     MOVE     OUT-CNT        TO   PARA-OUTCNT.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SKE0210B END   *** "
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
     READ     RCVFUTF
        AT    END
              MOVE      1    TO   END-FLG
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
     READ     RUIFUTF
        INVALID
              MOVE     "E"   TO   CHK-FLG
        NOT INVALID
              MOVE     "O"   TO   CHK-FLG
     END-READ.
*
 220-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
