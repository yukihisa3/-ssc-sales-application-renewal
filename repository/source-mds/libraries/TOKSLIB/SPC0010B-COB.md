# SPC0010B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SPC0010B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    業務名　　　　　　　：　ＰＣ連携                          *
*    モジュール名　　　　：　ＰＣ側受信終了Ｆ確認              *
*    作成日／更新日　　　：　2001/01/30                        *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　ＰＣ受信終了Ｆのチェック／確認を  *
*                        ：　行う。                            *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SPC0010B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          01/01/30.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       GP6000.
 OBJECT-COMPUTER.       GP6000.
 SPECIAL-NAMES.
     CONSOLE       IS        CONS
     STATION       IS        STAT.
****************************************************************
 INPUT-OUTPUT              SECTION.
****************************************************************
 FILE-CONTROL.
*----<<受信終了Ｆ>>----*
     SELECT   PCCHK1  ASSIGN         DA-01-S-PCCHK1
                        ORGANIZATION   SEQUENTIAL
                        STATUS         PC-ST.
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<<受信終了Ｆ>>----*
 FD  PCCHK1
                        BLOCK CONTAINS 1 RECORDS.
 01  PC-REC.
     03  PC-F01         PIC  9(08).
*
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  WK-CNT.
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
     03  PC-ST         PIC  X(02).
*
 01  PG-ID              PIC  X(08)     VALUE  "SPC0010B".
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
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN             PIC  X(01).
 01  LINK-IN-YMD6            PIC  9(06).
 01  LINK-IN-YMD8            PIC  9(08).
 01  LINK-OUT-RET            PIC  X(01).
 01  LINK-OUT-YMD            PIC  9(08).
 LINKAGE                SECTION.
 01  PARA-CHK                PIC  9(01).
 01  PARA-TOKCD              PIC  9(08).
****************************************************************
 PROCEDURE              DIVISION  USING  PARA-CHK PARA-TOKCD.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
 ACSERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      PCCHK1.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SPC0010B PCCHK1 ERROR " PC-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
     PERFORM  100-INIT-RTN.
     PERFORM  200-MAIN-RTN.
     PERFORM  300-END-RTN.
     STOP RUN.
 000-PROG-CNTL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｼｮｷ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 100-INIT-RTN           SECTION.
*クリア
     INITIALIZE    WK-CNT.
*
     DISPLAY  "*** SPC0010B START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
*
     OPEN     INPUT     PCCHK1.
*
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
*
     READ  PCCHK1 AT END
           MOVE   ZERO       TO   PARA-CHK
           GO                TO   200-MAIN-RTN-EXIT
     END-READ.
*
     IF       PC-F01  =  99999999
              MOVE   2       TO   PARA-CHK
           GO                TO   200-MAIN-RTN-EXIT
     END-IF.
*
     IF       PC-F01  =  88888888
              MOVE   4       TO   PARA-CHK
           GO                TO   200-MAIN-RTN-EXIT
     END-IF.
*
     IF       PC-F01  =  PARA-TOKCD
              MOVE   1       TO   PARA-CHK
     ELSE
              MOVE   3       TO   PARA-CHK
     END-IF.
*
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     CLOSE    PCCHK1.
*
 300-END-RTN-EXIT.
     EXIT.

```
