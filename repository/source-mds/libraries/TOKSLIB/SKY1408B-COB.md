# SKY1408B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SKY1408B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　：　サカタのタネ（株）　　　　　　　　　　*
*    業務名　　　　　：　スポットメモ番号　　　　　　　　　　　*
*    モジュール名　　：　条件Ｆメモ_更新　　　　　　　　　　　*
*    作成日／更新日　：　1999/10/21                            *
*    作成者／更新者　：　NAV                                   *
*                                                              *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SKY1408B.
 AUTHOR.                T.TAKAHASHI.
 DATE-WRITTEN.          99/10/21.
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       K-150SI.
 OBJECT-COMPUTER.       K-150SI.
 SPECIAL-NAMES.
     CONSOLE     IS     CONS
     STATION     IS     STAT.
***************************************************************
 INPUT-OUTPUT           SECTION.
***************************************************************
 FILE-CONTROL.
*----<< 条件ファイル >>-*
     SELECT   HJYOKEN   ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      RANDOM
                        RECORD    KEY       JYO-F01   JYO-F02
                        FILE      STATUS    JYO-ST1.
******************************************************************
 DATA                      DIVISION.
******************************************************************
 FILE                      SECTION.
*----<< 条件ファイル >>-*
 FD  HJYOKEN            BLOCK     CONTAINS   6   RECORDS
                        LABEL     RECORD     IS  STANDARD.
     COPY     HJYOKEN   OF   XFDLIB    JOINING   JYO  AS   PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  PGM-ID                  PIC  X(08)     VALUE  "SKY1408B".
 01  STATUS-AREA.
     03  IN-DATA             PIC  X(01).
     03  JYO-STATUS.
         05  JYO-ST1         PIC  X(02).
 01  FLAGS.
     03  END-FLG             PIC  9(01).
     03  JYO-OPEN-FLG        PIC  9(01).
******************************************************************
 LINKAGE                   SECTION.
 01  LINK-WS                 PIC  X(08).
******************************************************************
 PROCEDURE                 DIVISION  USING  LINK-WS.
******************************************************************
*--------------------------------------------------------------*
*    LEVEL   0     エラー処理　　　　　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 DECLARATIVES.
*----------   条件ファイル　-----------------------------------*
 JYO-ERR                     SECTION.
     USE         AFTER       EXCEPTION PROCEDURE   HJYOKEN.
     DISPLAY  "### " PGM-ID " " NC"条件ファイル異常！"
              "ST1=" JYO-ST1  " "
                                           " ###"  UPON STAT.
*
     MOVE     4000           TO   PROGRAM-STATUS.
     STOP     RUN.
 END DECLARATIVES.
*--------------------------------------------------------------*
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
*
     MOVE        ZERO      TO        FLAGS.
*条件ファイルオープン
     OPEN     I-O       HJYOKEN.
     MOVE     1              TO   JYO-OPEN-FLG.
*条件ファイル’５２’＋ﾜｰｸｽﾃｰｼｮﾝ名にて読込み
     MOVE     "52"           TO   JYO-F01.
     MOVE     LINK-WS        TO   JYO-F02.
     READ     HJYOKEN   INVALID
              DISPLAY "HJYOKEN INVALID KEY = " JYO-F01 ":" JYO-F02
                       UPON CONS
              STOP  RUN
     END-READ.
*開始メモ番号セット
     MOVE     JYO-F05    TO   JYO-F04.
*条件ファイル更新
     REWRITE  JYO-REC.
*プログラム終了
     STOP RUN.
 000-PROG-CNTL-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
