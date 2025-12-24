# SDL0010B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SDL0010B.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    業務名　　　　　　　：　販売管理システム　　　　　　　　　*
*    モジュール名　　　　：　在庫ファイル自動削除　　　　　　　*
*    作成日／更新日　　　：　05/09/20                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　現在個数～引当済数がすべて０のレコ*
*                            ードを削除する。                  *
*                                                              *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SDL0010B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          05/09/20.
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
*----<< 在庫データ >>--*
     SELECT   ZAMZAIF   ASSIGN         DA-01-VI-ZAMZAIL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  ZAI-F01   ZAI-F02
                                       ZAI-F03
                        STATUS         ZAI-ST1.
     SELECT   WKZAIF   ASSIGN         DA-01-S-WKZAIF
                        ORGANIZATION   SEQUENTIAL
                        STATUS         WKZ-ST1.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 在庫データ >>--*
 FD  ZAMZAIF            LABEL     RECORD   IS   STANDARD.
     COPY     ZAMZAIF   OF        XFDLIB
              JOINING   ZAI       PREFIX.
*----<< 削除データワーク>>--*
 FD  WKZAIF            LABEL     RECORD   IS   STANDARD.
     COPY     ZAMZAIF   OF        XFDLIB
              JOINING   WKZ       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  END-FLG        PIC  9(01).
     03  INV-FLG        PIC  9(01).
     03  DEL-FLG        PIC  9(01).
 01  COUNTERS.
     03  IN-CNT         PIC  9(06).
     03  OUT-CNT        PIC  9(06).
     03  DEL-CNT        PIC  9(06).
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  ZAI-ST1            PIC  X(02).
 01  WKZ-ST1            PIC  X(02).
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
*----<< 在庫データ >>--*
 ZAMZAIF-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      ZAMZAIF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SDL0010B ZAMZAIF ERROR " ZAI-ST1 " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     CLOSE    ZAMZAIF.
     CLOSE    WKZAIF.
     STOP     RUN.
*----<< 削除データワーク >>--*
 WKZAIF-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      WKZAIF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SDL0010B WKZAIF ERROR " WKZ-ST1 " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     CLOSE    ZAMZAIF.
     CLOSE    WKZAIF.
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
     DISPLAY  "*** SDL0010B START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS       UPON CONS.
     OPEN     I-O       ZAMZAIF.
     OPEN     OUTPUT    WKZAIF.
*----<< ﾜｰｸ ｼｮｷｾｯﾄ >>-*
     INITIALIZE         FLAGS.
     INITIALIZE         COUNTERS.
     INITIALIZE         WKZ-REC.
*
*    初期読込
     PERFORM   900-ZAI-READ.
*
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
     IF        DEL-FLG   =  1
               DELETE    ZAMZAIF   RECORD
               END-DELETE
               ADD       1    TO   DEL-CNT
               MOVE      ZERO TO   DEL-FLG
     END-IF.
     ADD       1         TO   OUT-CNT.
     PERFORM  900-ZAI-READ.
*
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
*
     CLOSE    ZAMZAIF
              WKZAIF.
*
     DISPLAY  "+++ ﾆｭｳﾘｮｸｹﾝｽｳ =" IN-CNT  " +++" UPON CONS.
     DISPLAY  "+++ ｻｸｼﾞｮ ｹﾝｽｳ =" DEL-CNT " +++" UPON CONS.
     DISPLAY  "+++ ｼｭﾂﾘｮｸｹﾝｽｳ =" OUT-CNT " +++" UPON CONS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SDL0010B END   *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS         UPON CONS.
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    在庫ファイル　 READ                          *
*--------------------------------------------------------------*
 900-ZAI-READ           SECTION.
     READ     ZAMZAIF   AT   END
              MOVE      1              TO   END-FLG
              GO   TO   900-ZAI-READ-EXIT
     END-READ.
*    現在個数～引当在庫数をチェック
     IF       ZAI-F04 = ZERO    AND    ZAI-F05 = ZERO
     AND      ZAI-F06 = ZERO    AND    ZAI-F07 = ZERO
     AND      ZAI-F08 = ZERO    AND    ZAI-F09 = ZERO
     AND      ZAI-F10 = ZERO    AND    ZAI-F20 = ZERO
     AND      ZAI-F26 = ZERO    AND    ZAI-F27 = ZERO
*****AND      ZAI-F14 = ZERO    AND    ZAI-F15 = ZERO
*****AND      ZAI-F16 = ZERO    AND    ZAI-F17 = ZERO
*****AND      ZAI-F18 = ZERO    AND    ZAI-F19 = ZERO
*****AND      ZAI-F20 = ZERO    AND    ZAI-F21 = ZERO
*****AND      ZAI-F22 = ZERO    AND    ZAI-F23 = ZERO
*****AND      ZAI-F24 = ZERO    AND    ZAI-F25 = ZERO
*****AND      ZAI-F26 = ZERO    AND    ZAI-F27 = ZERO
*****AND      ZAI-F28 = ZERO
              MOVE      1       TO     DEL-FLG
              MOVE      SPACE   TO     WKZ-REC
              INITIALIZE               WKZ-REC
              MOVE      ZAI-REC TO     WKZ-REC
              WRITE                    WKZ-REC
     END-IF.
*
     ADD      1         TO   IN-CNT.
*    件数表示
     IF       IN-CNT(4:3) = "000"  OR  "500"
              DISPLAY "IN-CNT = " IN-CNT UPON CONS
     END-IF.
*
 900-ZAI-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
