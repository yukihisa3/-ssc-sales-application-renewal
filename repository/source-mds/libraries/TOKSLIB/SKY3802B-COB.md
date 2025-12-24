# SKY3802B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SKY3802B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    業務名　　　　　　　：　_卸業務                          *
*    モジュール名　　　　：　電算室_卸データ振分け            *
*    作成日／更新日　　　：　2003/12/03                        *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　電算室から受信した_卸データを営業*
*                        ：　所Ｆへの振替を行う。              *
*                        ：　（組織変更対応）                  *
*    作成日／更新日　　　：　2013/06/14                        *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　_卸送信データＦが存在しない場合　*
*                        ：　本社Ｆへ出力する。　              *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SKY3802B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          03/12/03.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       GP6000.
 OBJECT-COMPUTER.       GP6000.
 SPECIAL-NAMES.
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<<_卸 データ >>--*
     SELECT   TANADT    ASSIGN         DA-01-S-TANADT
                        ORGANIZATION   SEQUENTIAL
                        STATUS         TAN1-ST.
*----<<_卸データ送信マスタ >>--*
     SELECT   RCVTANF   ASSIGN         DA-01-VI-RCVTANL1
                        ORGANIZATION   INDEXED
                        ACCESS   MODE  RANDOM
                        RECORD   KEY   RCV-F01
                                       RCV-F02
                        STATUS         RCV-ST.
*----<<本社 データ >>--*
     SELECT   TOK00     ASSIGN         DA-01-S-TOK00
                        ORGANIZATION   SEQUENTIAL
                        STATUS         TOK00-ST.
*----<<福岡 データ >>--*
     SELECT   TOK01     ASSIGN         DA-01-S-TOK01
                        ORGANIZATION   SEQUENTIAL
                        STATUS         TOK01-ST.
*----<<仙台 データ >>--*
     SELECT   TOK02     ASSIGN         DA-01-S-TOK02
                        ORGANIZATION   SEQUENTIAL
                        STATUS         TOK02-ST.
*----<<白河 データ >>--*
     SELECT   TOK03     ASSIGN         DA-01-S-TOK03
                        ORGANIZATION   SEQUENTIAL
                        STATUS         TOK03-ST.
*----<<岡山 データ >>--*
     SELECT   TOK04     ASSIGN         DA-01-S-TOK04
                        ORGANIZATION   SEQUENTIAL
                        STATUS         TOK04-ST.
*----<<北海道データ>>--*
     SELECT   TOK05     ASSIGN         DA-01-S-TOK05
                        ORGANIZATION   SEQUENTIAL
                        STATUS         TOK05-ST.
*----<<名古屋データ>>--*
     SELECT   TOK06     ASSIGN         DA-01-S-TOK06
                        ORGANIZATION   SEQUENTIAL
                        STATUS         TOK06-ST.
*----<<関東北データ>>--*
     SELECT   TOK07     ASSIGN         DA-01-S-TOK07
                        ORGANIZATION   SEQUENTIAL
                        STATUS         TOK07-ST.
*----<<横浜 データ >>--*
     SELECT   TOK08     ASSIGN         DA-01-S-TOK08
                        ORGANIZATION   SEQUENTIAL
                        STATUS         TOK08-ST.
*----<<大阪 データ >>--*
     SELECT   TOK09     ASSIGN         DA-01-S-TOK09
                        ORGANIZATION   SEQUENTIAL
                        STATUS         TOK09-ST.
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< _卸データ >>--*
 FD  TANADT            LABEL RECORD   IS   STANDARD.
     COPY     ZTANADT   OF        XFDLIB
              JOINING   TAN1      PREFIX.
*----<< _卸データ送信マスタ >>--*
 FD  RCVTANF           LABEL RECORD   IS   STANDARD.
     COPY     RCVTANF   OF        XFDLIB
              JOINING   RCV       PREFIX.
*----<< _卸データ >>--*
 FD  TOK00              BLOCK CONTAINS  3   RECORDS
                        LABEL RECORD   IS   STANDARD.
     COPY     ZTANADT   OF        XFDLIB
              JOINING   TOH0      PREFIX.
*----<< _卸データ >>--*
 FD  TOK01              BLOCK CONTAINS  3   RECORDS
                        LABEL RECORD   IS   STANDARD.
     COPY     ZTANADT   OF        XFDLIB
              JOINING   TOH1      PREFIX.
*----<< _卸データ >>--*
 FD  TOK02              BLOCK CONTAINS  3   RECORDS
                        LABEL RECORD   IS   STANDARD.
     COPY     ZTANADT   OF        XFDLIB
              JOINING   TOH2      PREFIX.
*----<< _卸データ >>--*
 FD  TOK03              BLOCK CONTAINS  3   RECORDS
                        LABEL RECORD   IS   STANDARD.
     COPY     ZTANADT   OF        XFDLIB
              JOINING   TOH3      PREFIX.
*----<< _卸データ >>--*
 FD  TOK04              BLOCK CONTAINS  3   RECORDS
                        LABEL RECORD   IS   STANDARD.
     COPY     ZTANADT   OF        XFDLIB
              JOINING   TOH4      PREFIX.
*----<< _卸データ >>--*
 FD  TOK05              BLOCK CONTAINS  3   RECORDS
                        LABEL RECORD   IS   STANDARD.
     COPY     ZTANADT   OF        XFDLIB
              JOINING   TOH5      PREFIX.
*----<< _卸データ >>--*
 FD  TOK06              BLOCK CONTAINS  3   RECORDS
                        LABEL RECORD   IS   STANDARD.
     COPY     ZTANADT   OF        XFDLIB
              JOINING   TOH6      PREFIX.
*----<< _卸データ >>--*
 FD  TOK07              BLOCK CONTAINS  3   RECORDS
                        LABEL RECORD   IS   STANDARD.
     COPY     ZTANADT   OF        XFDLIB
              JOINING   TOH7      PREFIX.
*----<< _卸データ >>--*
 FD  TOK08              BLOCK CONTAINS  3   RECORDS
                        LABEL RECORD   IS   STANDARD.
     COPY     ZTANADT   OF        XFDLIB
              JOINING   TOH8      PREFIX.
*----<< _卸データ >>--*
 FD  TOK09              BLOCK CONTAINS  3   RECORDS
                        LABEL RECORD   IS   STANDARD.
     COPY     ZTANADT   OF        XFDLIB
              JOINING   TOH9      PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  END-FLG        PIC  9(01).
     03  INV-FLG        PIC  9(01)   VALUE ZERO.
     03  RCVTANF-INV-FLG PIC  X(03)  VALUE SPACE.
 01  WK-CNT.
     03  ERR-CNT        PIC  9(07).
     03  IN-CNT         PIC  9(07).
     03  TOK00-CNT      PIC  9(07).
     03  TOK01-CNT      PIC  9(07).
     03  TOK02-CNT      PIC  9(07).
     03  TOK03-CNT      PIC  9(07).
     03  TOK04-CNT      PIC  9(07).
     03  TOK05-CNT      PIC  9(07).
     03  TOK06-CNT      PIC  9(07).
     03  TOK07-CNT      PIC  9(07).
     03  TOK08-CNT      PIC  9(07).
     03  TOK09-CNT      PIC  9(07).
     03  WK-FILE-NO     PIC  9(02).
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  TAN1-ST           PIC  X(02).
 01  RCV-ST            PIC  X(02).
 01  TOK00-ST          PIC  X(02).
 01  TOK01-ST          PIC  X(02).
 01  TOK02-ST          PIC  X(02).
 01  TOK03-ST          PIC  X(02).
 01  TOK04-ST          PIC  X(02).
 01  TOK05-ST          PIC  X(02).
 01  TOK06-ST          PIC  X(02).
 01  TOK07-ST          PIC  X(02).
 01  TOK08-ST          PIC  X(02).
 01  TOK09-ST          PIC  X(02).
*
 01  WK-BUMON          PIC  9(03)      VALUE  ZERO.
 01  HEN-BUMON.
     03  HEN-BUMON1     PIC  9(03).
     03  HEN-BUMON2     PIC  9(01).
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
*----<< ワークＦ >>--*
 TANADT                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      TANADT.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SKY3802B TANADT ERROR " TAN1-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< _卸データ送信マスタ >>--*
 RCVTANF               SECTION.
     USE AFTER     EXCEPTION PROCEDURE      RCVTANF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SKY3802B RCVTANF ERROR " RCV-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< ワークＦ >>--*
 TOK00                  SECTION.
     USE AFTER     EXCEPTION PROCEDURE      TOK00.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SKY3802B TOK00   ERROR " TOK00-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 TOK01                  SECTION.
     USE AFTER     EXCEPTION PROCEDURE      TOK01.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SKY3802B TOK01   ERROR " TOK01-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 TOK02                  SECTION.
     USE AFTER     EXCEPTION PROCEDURE      TOK02.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SKY3802B TOK02   ERROR " TOK02-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 TOK03                  SECTION.
     USE AFTER     EXCEPTION PROCEDURE      TOK03.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SKY3802B TOK03   ERROR " TOK03-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 TOK04                  SECTION.
     USE AFTER     EXCEPTION PROCEDURE      TOK04.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SKY3802B TOK04   ERROR " TOK04-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 TOK05                  SECTION.
     USE AFTER     EXCEPTION PROCEDURE      TOK05.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SKY3802B TOK05   ERROR " TOK05-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 TOK06                  SECTION.
     USE AFTER     EXCEPTION PROCEDURE      TOK06.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SKY3802B TOK06   ERROR " TOK06-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 TOK07                  SECTION.
     USE AFTER     EXCEPTION PROCEDURE      TOK07.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SKY3802B TOK07   ERROR " TOK07-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 TOK08                  SECTION.
     USE AFTER     EXCEPTION PROCEDURE      TOK08.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SKY3802B TOK08   ERROR " TOK08-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 TOK09                  SECTION.
     USE AFTER     EXCEPTION PROCEDURE      TOK09.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SKY3802B TOK09   ERROR " TOK09-ST " "
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
     PERFORM  200-MAIN-RTN   UNTIL     END-FLG   =    9.
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
     DISPLAY  "*** SKY3802B START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     INPUT     TANADT  RCVTANF.
     OPEN     OUTPUT    TOK00 TOK01 TOK02 TOK03 TOK04
                        TOK05 TOK06 TOK07 TOK08 TOK09.
*クリア
     INITIALIZE         FLAGS  WK-CNT  HEN-BUMON.
*_卸Ｆリード
     PERFORM       TANADT-RD-SEC.
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
*振分先判定
*****MOVE      TAN1-F02          TO    WK-BUMON.
     EVALUATE  WK-FILE-NO
         WHEN  0
               MOVE     SPACE    TO    TOH0-REC
               INITIALIZE              TOH0-REC
               MOVE     TAN1-REC TO    TOH0-REC
               WRITE    TOH0-REC
               ADD      1        TO    TOK00-CNT
         WHEN  1
               MOVE     SPACE    TO    TOH1-REC
               INITIALIZE              TOH1-REC
               MOVE     TAN1-REC TO    TOH1-REC
               WRITE    TOH1-REC
               ADD      1        TO    TOK01-CNT
         WHEN  2
               MOVE     SPACE    TO    TOH2-REC
               INITIALIZE              TOH2-REC
               MOVE     TAN1-REC TO    TOH2-REC
               WRITE    TOH2-REC
               ADD      1        TO    TOK02-CNT
         WHEN  3
               MOVE     SPACE    TO    TOH3-REC
               INITIALIZE              TOH3-REC
               MOVE     TAN1-REC TO    TOH3-REC
               WRITE    TOH3-REC
               ADD      1        TO    TOK03-CNT
         WHEN  4
               MOVE     SPACE    TO    TOH4-REC
               INITIALIZE              TOH4-REC
               MOVE     TAN1-REC TO    TOH4-REC
               WRITE    TOH4-REC
               ADD      1        TO    TOK04-CNT
         WHEN  5
               MOVE     SPACE    TO    TOH5-REC
               INITIALIZE              TOH5-REC
               MOVE     TAN1-REC TO    TOH5-REC
               WRITE    TOH5-REC
               ADD      1        TO    TOK05-CNT
         WHEN  6
               MOVE     SPACE    TO    TOH6-REC
               INITIALIZE              TOH6-REC
               MOVE     TAN1-REC TO    TOH6-REC
               WRITE    TOH6-REC
               ADD      1        TO    TOK06-CNT
         WHEN  7
               MOVE     SPACE    TO    TOH7-REC
               INITIALIZE              TOH7-REC
               MOVE     TAN1-REC TO    TOH7-REC
               WRITE    TOH7-REC
               ADD      1        TO    TOK07-CNT
         WHEN  8
               MOVE     SPACE    TO    TOH8-REC
               INITIALIZE              TOH8-REC
               MOVE     TAN1-REC TO    TOH8-REC
               WRITE    TOH8-REC
               ADD      1        TO    TOK08-CNT
         WHEN  9
               MOVE     SPACE    TO    TOH9-REC
               INITIALIZE              TOH9-REC
               MOVE     TAN1-REC TO    TOH9-REC
               WRITE    TOH9-REC
               ADD      1        TO    TOK09-CNT
         WHEN  OTHER
               MOVE     SPACE    TO    TOH8-REC
               INITIALIZE              TOH8-REC
               MOVE     TAN1-REC TO    TOH8-REC
               WRITE    TOH8-REC
               ADD      1        TO    TOK08-CNT
               ADD      1        TO    ERR-CNT
     END-EVALUATE.
*_卸Ｆリード
     PERFORM       TANADT-RD-SEC.
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     CLOSE    TANADT TOK00 TOK01 TOK02 TOK03 TOK04
                      TOK05 TOK06 TOK07 TOK08 TOK09.
*
     DISPLAY  NC"確定_卸ＤＴ全件数＝" IN-CNT    UPON CONS.
     DISPLAY  NC"振分先なし　　件数＝" ERR-CNT   UPON CONS.
     DISPLAY  NC"本社Ｄ振分　　件数＝" TOK00-CNT UPON CONS.
     DISPLAY  NC"福岡Ｄ振分　　件数＝" TOK01-CNT UPON CONS.
     DISPLAY  NC"仙台Ｄ振分　　件数＝" TOK02-CNT UPON CONS.
     DISPLAY  NC"白河Ｄ振分　　件数＝" TOK03-CNT UPON CONS.
     DISPLAY  NC"岡山Ｄ振分　　件数＝" TOK04-CNT UPON CONS.
     DISPLAY  NC"北海道Ｄ振分　件数＝" TOK05-CNT UPON CONS
     DISPLAY  NC"名古屋Ｄ振分　件数＝" TOK06-CNT UPON CONS.
     DISPLAY  NC"北関東Ｄ振分　件数＝" TOK07-CNT UPON CONS.
     DISPLAY  NC"エラー振分　　件数＝" TOK08-CNT UPON CONS.
     DISPLAY  NC"大阪Ｄ振分　　件数＝" TOK09-CNT UPON CONS.

     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SKY3802B END *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4      _卸Ｆリード                                *
*--------------------------------------------------------------*
 TANADT-RD-SEC         SECTION.
     READ   TANADT  AT  END
            MOVE     9   TO     END-FLG
            GO           TO     TANADT-RD-EXIT
            NOT  AT  END
            ADD      1   TO     IN-CNT
     END-READ.
*
     MOVE   TAN1-F02X    TO     RCV-F01.
     MOVE   TAN1-F04     TO     RCV-F02.
     READ   RCVTANF
            INVALID      MOVE   "INV"    TO   RCVTANF-INV-FLG
            NOT INVALID  MOVE   SPACE    TO   RCVTANF-INV-FLG
     END-READ.
*
     IF     RCVTANF-INV-FLG  =  "INV"
*2013/06/14 NAV ST 存在しない場合、９９ではなく００：本社に出力
************MOVE    99       TO  WK-FILE-NO
            MOVE    ZERO     TO  WK-FILE-NO
*2013/06/14 NAV ED 存在しない場合、９９ではなく００：本社に出力
     ELSE
            MOVE    RCV-F03  TO  WK-FILE-NO
     END-IF.
*
 TANADT-RD-EXIT.
     EXIT.

```
