# SZE0020B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SZE0020B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    業務名　　　　　　　：　全社在庫管理システム　　　　　　　*
*    モジュール名　　　　：　全社在庫データ集計　　　　　　　　*
*    作成日／更新日　　　：　2003/07/08                        *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　各営業所の在庫を読み、全社在庫　　*
*                        ：　マスタへ出力する。                *
*                        ：　                                  *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SZE0020B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          03/07/08.
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
*----<<部門＝３２６６　在庫Ｆ（本社）>>----*
     SELECT   HOU       ASSIGN         DA-01-S-HOU
                        ORGANIZATION   SEQUENTIAL
                        STATUS         HOU-ST.
*----<<部門＝３４３６　在庫Ｆ（福岡）>>----*
     SELECT   FKU       ASSIGN         DA-01-S-FKU
                        ORGANIZATION   SEQUENTIAL
                        STATUS         FKU-ST.
*----<<部門＝３２３６　在庫Ｆ（仙台）>>----*
     SELECT   SEU       ASSIGN         DA-01-S-SEU
                        ORGANIZATION   SEQUENTIAL
                        STATUS         SEU-ST.
*----<<部門＝３３５６　在庫Ｆ（岡山）>>----*
     SELECT   OKU       ASSIGN         DA-01-S-OKU
                        ORGANIZATION   SEQUENTIAL
                        STATUS         SEU-ST.
*----<<部門＝３１３６　在庫Ｆ（北海道）>>----*
     SELECT   HKU       ASSIGN         DA-01-S-HKU
                        ORGANIZATION   SEQUENTIAL
                        STATUS         HKU-ST.
*----<<部門＝３３４６　在庫Ｆ（大阪）>>----*
     SELECT   OSU       ASSIGN         DA-01-S-OSU
                        ORGANIZATION   SEQUENTIAL
                        STATUS         OSU-ST.
*----<<全社在庫マスタ>>----*
     SELECT   ZENZAIF   ASSIGN    TO        ZENZAIF
                        FILE      STATUS    IS   ZEN-ST.
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<<部門＝３２６６　在庫Ｆ（本社）>>----*
 FD  HOU
                        BLOCK CONTAINS 1 RECORDS.
 01  HOU-REC            PIC  X(300).
*----<<部門＝３４３６　在庫Ｆ（福岡）>>----*
 FD  FKU
                        BLOCK CONTAINS 1 RECORDS.
 01  FKU-REC            PIC  X(300).
*----<<部門＝３２３６　在庫Ｆ（仙台）>>----*
 FD  SEU
                        BLOCK CONTAINS 1 RECORDS.
 01  SEU-REC            PIC  X(300).
*----<<部門＝３３５６　在庫Ｆ（岡山）>>----*
 FD  OKU
                        BLOCK CONTAINS 1 RECORDS.
 01  OKU-REC            PIC  X(300).
*----<<部門＝３１３６　在庫Ｆ（北海道）>>----*
 FD  HKU
                        BLOCK CONTAINS 1 RECORDS.
 01  HKU-REC            PIC  X(300).
*----<<部門＝３３４６　在庫Ｆ（大阪)----*
 FD  OSU
                        BLOCK CONTAINS 1 RECORDS.
 01  OSU-REC            PIC  X(300).
*----<<全社在庫マスタ>>----*
 FD  ZENZAIF
     LABEL    RECORD    IS        STANDARD.
     COPY     ZENZAIF   OF        XFDLIB
              JOINING   ZEN       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  HOU-FLG        PIC  X(03)   VALUE SPACE.
     03  FKU-FLG        PIC  X(03)   VALUE SPACE.
     03  SEU-FLG        PIC  X(03)   VALUE SPACE.
     03  OKU-FLG        PIC  X(03)   VALUE SPACE.
     03  HKU-FLG        PIC  X(03)   VALUE SPACE.
     03  OSU-FLG        PIC  X(03)   VALUE SPACE.
 01  WK-CNT.
     03  HOU-CNT        PIC  9(07).
     03  FKU-CNT        PIC  9(07).
     03  SEU-CNT        PIC  9(07).
     03  OKU-CNT        PIC  9(07).
     03  HKU-CNT        PIC  9(07).
     03  OSU-CNT        PIC  9(07).
     03  ZEN-CNT        PIC  9(07).
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
     03  HOU-ST         PIC  X(02).
     03  FKU-ST         PIC  X(02).
     03  OKU-ST         PIC  X(02).
     03  SEU-ST         PIC  X(02).
     03  HKU-ST         PIC  X(02).
     03  OSU-ST         PIC  X(02).
     03  ZEN-ST         PIC  X(02).
*
 01  PG-ID             PIC  X(08)      VALUE  "SZE0020B".
*----<< ﾋﾂﾞｹ ﾜｰｸ >>--*
 01  SYS-YYMD           PIC  9(08).
 01  FILLER             REDEFINES      SYS-YYMD.
     03  SYS-YYYY       PIC  9(04).
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
****************************************************************
 PROCEDURE              DIVISION.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
 HOUERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HOU.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SZE0020B HOU ERROR " HOU-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     MOVE     4000          TO    PROGRAM-STATUS.
     STOP     RUN.
 FKUERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      FKU.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SZE0020B FKU ERROR " FKU-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     MOVE     4000          TO    PROGRAM-STATUS.
     STOP     RUN.
 SEUERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SEU.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SZE0020B SEU ERROR " SEU-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     MOVE     4000          TO    PROGRAM-STATUS.
     STOP     RUN.
 HKUERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HKU.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SZE0020B SEU ERROR " SEU-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     MOVE     4000          TO    PROGRAM-STATUS.
     STOP     RUN.
 OSUERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      OSU.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SZE0020B OSU ERROR " OSU-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     MOVE     4000          TO    PROGRAM-STATUS.
     STOP     RUN.
 PRTERR                 SECTION.
     USE AFTER     EXCEPTION PROCEDURE      ZENZAIF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SZE0020B ZENZAIF ERROR " ZEN-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     MOVE     4000          TO    PROGRAM-STATUS.
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
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SZE0020B START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     INPUT     HOU FKU OKU SEU HKU OSU.
     OPEN     OUTPUT    ZENZAIF.
*クリア
     INITIALIZE    WK-CNT  FLAGS.
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
*本社件数カウント
*    売上件数カウント
     PERFORM HOU-RD-SEC  UNTIL  HOU-FLG = "END".
*福岡件数カウント
*    売上件数カウント
     PERFORM FKU-RD-SEC  UNTIL  FKU-FLG = "END".
*仙台件数カウント
*    売上件数カウント
     PERFORM SEU-RD-SEC  UNTIL  SEU-FLG = "END".
*岡山件数カウント
*    売上件数カウント
     PERFORM OKU-RD-SEC  UNTIL  OKU-FLG = "END".
*北海道件数カウント
*    売上件数カウント
     PERFORM HKU-RD-SEC  UNTIL  HKU-FLG = "END".
*大阪件数カウント
*    売上件数カウント
     PERFORM OSU-RD-SEC  UNTIL  OSU-FLG = "END".
*
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     CLOSE    HOU FKU SEU OKU HKU OSU ZENZAIF.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SZE0020B END *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*                  在庫マスタ読込み                        *
*--------------------------------------------------------------*
 HOU-RD-SEC             SECTION.
     READ   HOU   AT  END
            MOVE  "END"  TO  HOU-FLG
            GO    TO     HOU-RD-EXIT
            NOT   AT  END
            ADD    1     TO  HOU-CNT
     END-READ.
     MOVE   SPACE        TO  ZEN-REC.
     MOVE   HOU-REC      TO  ZEN-REC.
     WRITE  ZEN-REC.
     ADD    1      TO    ZEN-CNT.
 HOU-RD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*                  在庫マスタ読込み                            *
*--------------------------------------------------------------*
 FKU-RD-SEC             SECTION.
     READ   FKU   AT  END
            MOVE  "END"  TO  FKU-FLG
            GO    TO     FKU-RD-EXIT
            NOT   AT  END
            ADD    1     TO  FKU-CNT
     END-READ.
     MOVE   SPACE        TO  ZEN-REC.
     MOVE   FKU-REC      TO  ZEN-REC.
     WRITE  ZEN-REC.
     ADD    1      TO    ZEN-CNT.
 FKU-RD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*                  在庫マスタ読込み                            *
*--------------------------------------------------------------*
 SEU-RD-SEC             SECTION.
     READ   SEU   AT  END
            MOVE  "END"  TO  SEU-FLG
            GO    TO     SEU-RD-EXIT
            NOT   AT  END
            ADD    1     TO  SEU-CNT
     END-READ.
     MOVE   SPACE        TO  ZEN-REC.
     MOVE   SEU-REC      TO  ZEN-REC.
     WRITE  ZEN-REC.
     ADD    1      TO    ZEN-CNT.
 SEU-RD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*                  在庫マスタ読込み                            *
*--------------------------------------------------------------*
 OKU-RD-SEC             SECTION.
     READ   OKU   AT  END
            MOVE  "END"  TO  OKU-FLG
            GO    TO     OKU-RD-EXIT
            NOT   AT  END
            ADD    1     TO  OKU-CNT
     END-READ.
     MOVE   SPACE        TO  ZEN-REC.
     MOVE   OKU-REC      TO  ZEN-REC.
     WRITE  ZEN-REC.
     ADD    1      TO    ZEN-CNT.
 OKU-RD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*                  在庫マスタ読込み                            *
*--------------------------------------------------------------*
 HKU-RD-SEC             SECTION.
     READ   HKU   AT  END
            MOVE  "END"  TO  HKU-FLG
            GO    TO     HKU-RD-EXIT
            NOT   AT  END
            ADD    1     TO  HKU-CNT
     END-READ.
     MOVE   SPACE        TO  ZEN-REC.
     MOVE   HKU-REC      TO  ZEN-REC.
     WRITE  ZEN-REC.
     ADD    1      TO    ZEN-CNT.
 HKU-RD-EXIT.
     EXIT.
*--------------------------------------------------------------*
*                  在庫マスタ読込み                            *
*--------------------------------------------------------------*
 OSU-RD-SEC             SECTION.
     READ   OSU   AT  END
            MOVE  "END"  TO  OSU-FLG
            GO    TO     OSU-RD-EXIT
            NOT   AT  END
            ADD    1     TO  OSU-CNT
     END-READ.
     MOVE   SPACE        TO  ZEN-REC.
     MOVE   OSU-REC      TO  ZEN-REC.
     WRITE  ZEN-REC.
     ADD    1      TO    ZEN-CNT.
 OSU-RD-EXIT.
     EXIT.

```
