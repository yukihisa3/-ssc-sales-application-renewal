# SGE0010B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SGE0010B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    業務名　　　　　　　：　販売管理システム　　　　　　　　　*
*    モジュール名　　　　：　伝票ファイル削除　　　　　　　　　*
*    作成日／更新日　　　：　92/12/05                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　伝票ファイルの請求区分が９のものを*
*                        ：　削除する。　また条件ファイルの月を*
*                        ：　カウントアップする。　　　　　　　*
*    2012.03.01 伝票Ｆキー変更対応 NAV TAKAHASHI               *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SGE0010B.
 AUTHOR.                S.K  SANKYO.
 DATE-WRITTEN.          92/12/05.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM-K150.
 OBJECT-COMPUTER.       FACOM-K150.
 SPECIAL-NAMES.
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 伝票データ >>--*
     SELECT   HDENJNL   ASSIGN         DA-01-VI-DENJNL4
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  DEN-F01   DEN-F02
***************************************DEN-F04   DEN-F03
                                       DEN-F04   DEN-F07
                                       DEN-F112  DEN-F03
                        STATUS         DEN-ST1.
*----<< 条件ファイル >>--*
     SELECT   HJYOKEN   ASSIGN         DA-01-VI-JYOKEN1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  JYO-F01   JYO-F02
                        STATUS         JYO-ST1.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 伝票データ >>--*
 FD  HDENJNL            LABEL     RECORD   IS   STANDARD.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   DEN       PREFIX.
*----<< 条件ファイル >>--*
 FD  HJYOKEN            LABEL RECORD   IS   STANDARD.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYO       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  END-FLG        PIC  9(01).
     03  INV-FLG        PIC  9(01).
 01  COUNTERS.
     03  IN-CNT         PIC  9(06).
     03  OUT-CNT        PIC  9(06).
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  DEN-ST.
     03  DEN-ST1        PIC  X(02).
     03  DEN-ST2        PIC  X(04).
 01  JYO-ST.
     03  JYO-ST1        PIC  X(02).
     03  JYO-ST2        PIC  X(04).
*
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
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
****************************************************************
 PROCEDURE              DIVISION.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 伝票データ >>--*
 HDENJNL-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HDENJNL.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SGE0010B HDENJNL ERROR " DEN-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     CLOSE    HDENJNL   HJYOKEN.
     STOP     RUN.
*----<< 条件ファイル >>--*
 HJYOKEN-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HJYOKEN.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SGE0010B HJYOKEN ERROR " JYO-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     CLOSE    HDENJNL   HJYOKEN.
     STOP     RUN.
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
     PERFORM  100-INIT-RTN.
     PERFORM  200-MAIN-RTN   UNTIL     END-FLG   =    1.
     PERFORM  300-END-RTN.
     STOP RUN.
 000-PROG-CNTL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｼｮｷ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 100-INIT-RTN           SECTION.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SGE0010B START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS       UPON CONS.
     OPEN     I-O       HDENJNL.
     OPEN     I-O       HJYOKEN.
*----<< ﾜｰｸ ｼｮｷｾｯﾄ >>-*
     INITIALIZE         FLAGS.
     INITIALIZE         COUNTERS.
*
     MOVE     58        TO   JYO-F01.
     MOVE     SPACE     TO   JYO-F02.
     PERFORM  900-JYO-READ.
     IF       INV-FLG   =    1
              MOVE      1         TO   END-FLG
     ELSE
              PERFORM   900-DEN-READ
     END-IF.
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
*\\ 93.06.11
*****IF       DEN-F261  =    9
     IF       DEN-F261  =    1
              DELETE    HDENJNL   RECORD
              END-DELETE
              ADD       1    TO   OUT-CNT
**** ELSE
****          REWRITE   DEN-REC
****          END-REWRITE
     END-IF.
*
     PERFORM  900-DEN-READ.
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     ADD      1         TO   JYO-F04.
     IF       JYO-F04   >    12
              MOVE      1         TO   JYO-F04
     END-IF.
     IF       INV-FLG   =    0
              REWRITE   JYO-REC
     END-IF.
*
     CLOSE    HJYOKEN.
     CLOSE    HDENJNL.
*
     DISPLAY  "+++ ﾆｭｳﾘｮｸｹﾝｽｳ =" IN-CNT  " +++" UPON CONS.
     DISPLAY  "+++ ｻｸｼﾞｮ ｹﾝｽｳ =" OUT-CNT " +++" UPON CONS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SGE0010B END   *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS         UPON CONS.
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    条件ファイル　 READ                          *
*--------------------------------------------------------------*
 900-JYO-READ           SECTION.
     MOVE     0         TO   INV-FLG.
     READ     HJYOKEN   INVALID
              MOVE      1              TO   INV-FLG
     DISPLAY  "### OSKT340 HJYOKEN INVALID KEY=" JYO-F01 " ###"
                                       UPON CONS
     END-READ.
 900-JYO-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    伝票ファイル　 READ                          *
*--------------------------------------------------------------*
 900-DEN-READ           SECTION.
     READ     HDENJNL   AT   END
              MOVE      1              TO   END-FLG
              GO   TO   900-DEN-READ-EXIT
     END-READ.
*
     ADD      1         TO   IN-CNT.
*    件数表示
     IF       IN-CNT(4:3) = "000"
              DISPLAY "IN-CNT = " IN-CNT UPON CONS
     END-IF.
*
 900-DEN-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
