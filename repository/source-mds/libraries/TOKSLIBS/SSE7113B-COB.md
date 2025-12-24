# SSE7113B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSE7113B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　Ｊ本田オンラインシステム　　　　　*
*    モジュール名　　　　：　請求データ作成　　　　　　　　　　*
*    作成日／更新日　　　：　17/04/03                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　サカタ請求データを、Ｊ本田請求Ｆへ*
*                        ：　データ出力する。　　　　　　　　　*
*    作成日／更新日　　　：　08/08/28                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　内部統制対応　　　　　　　　　　　*
*    作成日／更新日　　　：　17/04/03                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　店舗４桁対応　　　　　　　　　　　*
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSE7113B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          05/08/16.
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
*----<< 仕入実績集計データ >>--*
     SELECT   JHSEIKF   ASSIGN         DA-01-VI-JHSEIKL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  SEK-F01   SEK-F02
                                       SEK-F03
                        STATUS         SEK-ST.
*----<< 仕入実績請求データ >>--*
     SELECT   JHJSEKF   ASSIGN         DA-01-VI-JHJSEKL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  SEI-F01   SEI-F02
                                       SEI-F03
                        STATUS         SEI-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 伝票データ >>--*
 FD  JHSEIKF            LABEL     RECORD   IS   STANDARD.
     COPY     JHSEIKF   OF        XFDLIB
              JOINING   SEK       PREFIX.
*----<< 仕入実績集計データ >>--*
 FD  JHJSEKF            LABEL     RECORD   IS   STANDARD.
     COPY     JHJSEKF   OF        XFDLIB
              JOINING   SEI       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  END-FLG        PIC  9(01).
     03  INV-FLG        PIC  9(01).
     03  WRT-FLG        PIC  9(01).
 01  COUNTERS.
     03  DEN-CNT        PIC  9(06).
     03  OUT-CNT        PIC  9(06).
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  SEK-ST             PIC  X(02).
 01  SEI-ST             PIC  X(02).
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
 01  SIME-DATE          PIC  9(08).
 01  FILLER             REDEFINES      SIME-DATE.
     03  SIME-YY        PIC  9(04).
     03  SIME-MM        PIC  9(02).
     03  SIME-DD        PIC  9(02).
 01  SSKTLSTD-DATE      PIC  9(08).
 01  FILLER             REDEFINES      SSKTLSTD-DATE.
     03  SSKTLSTD-YY    PIC  9(04).
     03  SSKTLSTD-MM    PIC  9(02).
     03  SSKTLSTD-DD    PIC  9(02).
 01  SSKTLSTD-RET       PIC  9(01).
*金額計算用ワーク
 01  WK-KINGAKU         PIC  S9(10).
*合計金額計算用ワーク
 01  WK-GOUKEI          PIC  S9(10).
*数量ワーク
 01  WK-SURYO           PIC  S9(10).
 01  IND                PIC  99.
*
*----<< BREAK KEY >>--*
 01  BREAK-KEY.
     03  NEW.
         05  NEW-SIM                        PIC  9(08).
         05  NEW-TEN                        PIC  9(03).
         05  NEW-DEN                        PIC  9(07).
         05  NEW-GYO                        PIC  9(02).
     03  OLD.
         05  OLD-SIM                        PIC  9(08).
         05  OLD-TEN                        PIC  9(03).
         05  OLD-DEN                        PIC  9(07).
         05  OLD-GYO                        PIC  9(02).
*
 LINKAGE                SECTION.
 01  PARA-OUT-CNT                           PIC  9(07).
****************************************************************
 PROCEDURE              DIVISION  USING  PARA-OUT-CNT.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 伝票データ >>--*
 JHSEIKF-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      JHSEIKF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SSE7113B JHSEIKF ERROR " SEK-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 仕入実績請求Ｆ >>--*
 JHJSEKF-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      JHJSEKF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### OSKT320 JHJSEKF ERROR " SEI-ST " "
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
     DISPLAY  "*** SSE7113B START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     INPUT     JHSEIKF.
     OPEN     I-O       JHJSEKF.
*----<< ﾜｰｸ ｼｮｷｾｯﾄ >>-*
     INITIALIZE         FLAGS.
     INITIALIZE         COUNTERS.
*
     PERFORM  220-SEK-READ.
*
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
*
     PERFORM  220-SEI-OUT.
*
     IF       WRT-FLG  =  0
              MOVE    SPACE   TO   SEI-REC
              INITIALIZE           SEI-REC
              MOVE    SEK-REC TO   SEI-REC
              WRITE   SEI-REC
              ADD     1       TO   OUT-CNT
     END-IF.
*
     PERFORM  220-SEK-READ.
*
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     CLOSE    JHSEIKF.
     CLOSE    JHJSEKF.
*
     DISPLAY  "+++ ﾃﾞﾝﾋﾟｮｳﾃﾞｰﾀ=" DEN-CNT " +++" UPON CONS.
     DISPLAY  "+++ OUTPUT     =" OUT-CNT " +++" UPON CONS.
*内部統制対応 2008/08/28
     MOVE     OUT-CNT        TO   PARA-OUT-CNT.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSE7113B END *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4      ｾｲｷｭｳ ﾃﾞｰﾀ ｼｭﾂﾘｮｸ                           *
*--------------------------------------------------------------*
 220-SEI-OUT            SECTION.
*
     MOVE     SEK-F01        TO   SEI-F01.
     MOVE     SEK-F02        TO   SEI-F02.
     MOVE     SEK-F03        TO   SEI-F03.
     READ     JHJSEKF        INVALID
              MOVE      0    TO   WRT-FLG
              NOT            INVALID
              MOVE      1    TO   WRT-FLG
     END-READ.
*
 220-SEI-OUT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4      ｾｲｷｭｳ ﾃﾞｰﾀ ｼｭﾂﾘｮｸ                           *
*--------------------------------------------------------------*
 220-SEK-READ           SECTION.
*
     READ     JHSEIKF        AT  END
              MOVE      1    TO  END-FLG
              NOT  AT  END
              ADD       1    TO  DEN-CNT
     END-READ.
*
 220-SEK-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
