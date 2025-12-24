# SSKT190

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIB/SSKT190.COB`

## ソースコード

```cobol
****************************************************************
*新基幹システムへ移行 1999/10/16 T.TAKAHASHI                   *
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    業務名　　　　　　　：　販売管理システム　　　　　　　　　*
*    モジュール名　　　　：　売上Ｄ作成ＦＬＧゼロ更新　　　　　*
*    作成日／更新日　　　：　92/11/25                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　売上Ｄより伝票Ｄを索引し　　　　　*
*                        ：　伝票Ｄの売上Ｄ作成ＦＬＧを　　　　*
*                        ：　ゼロクリアする　　　　　　　　　　*
*                                                              *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            OSKT190.
 AUTHOR.                T.A.
 DATE-WRITTEN.          93/03/04.
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
     SELECT   HDENJNL   ASSIGN         DA-01-VI-DENJNL3
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  DEN-F24   DEN-F23
                                       DEN-F04   DEN-F051
                                       DEN-F03
                        STATUS         HDENJNL-ST.
*----<<入庫データ>>--*
     SELECT   ZNYUKDT   ASSIGN         DA-01-VI-ZNYUKDT2
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  NYU-F02
                                       NYU-F03
                                       NYU-F04
                                       NYU-F05.
*----<<入出庫データ>>--*
     SELECT   ZNYUSDT   ASSIGN         DA-01-VI-ZNYUSDT1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE DYNAMIC
                        RECORD    KEY  NYS-F02
                                       NYS-F03.
*----<<作業表データ>>--*
     SELECT   ZSGYODT   ASSIGN         DA-01-VI-ZSGYODT1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE DYNAMIC
                        RECORD    KEY  SGY-F01
                                       SGY-F02.
*----<<     データ >>--*
     SELECT   TOKU      ASSIGN         DA-01-S-TOKU
                        ORGANIZATION   SEQUENTIAL
                        STATUS         TOKU-ST.
*----<< ワークＦ >>--*
     SELECT   HKYOTU    ASSIGN         DA-01-S-HKYOTU
                        ORGANIZATION   SEQUENTIAL
                        STATUS         KYO-ST.
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 伝票データ >>--*
 FD  HDENJNL            LABEL     RECORD   IS   STANDARD.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   DEN       PREFIX.
*----<<            >>--*
 FD  ZNYUKDT            LABEL     RECORD   IS   STANDARD.
     COPY     ZNYUKDT   OF        XFDLIB
              JOINING   NYU       PREFIX.
*----<<            >>--*
 FD  ZSGYODT            LABEL     RECORD   IS   STANDARD.
     COPY     ZSGYODT   OF        XFDLIB
              JOINING   SGY       PREFIX.
*----<<            >>--*
 FD  ZNYUSDT            LABEL     RECORD   IS   STANDARD.
     COPY     ZNYUSDT   OF        XFDLIB
              JOINING   NYS       PREFIX.
*----<< 売上データ >>--*
 FD  TOKU               LABEL RECORD   IS   STANDARD.
     COPY     TOKUREC   OF        XFDLIB
              JOINING   TOKU      PREFIX.
*----<< ワークＦ >>--*
 FD  HKYOTU             LABEL RECORD   IS   STANDARD.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   KYO       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  END-FLG        PIC  9(01).
     03  INV-FLG        PIC  9(01)   VALUE ZERO.
 01  WK-CNT.
     03  TOKU-CNT       PIC  9(07).
     03  DEN-CNT        PIC  9(07).
     03  ERR-CNT        PIC  9(07).
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  HDENJNL-ST        PIC  X(02).
 01  TOKU-ST           PIC  X(02).
 01  KYO-ST            PIC  X(02).
*
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
*----<< 伝票データ >>--*
 HDENJNL-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HDENJNL.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### OSKT190 HDENJNL ERROR " HDENJNL-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     CLOSE    TOKU.
     CLOSE    HDENJNL.
     CLOSE    HKYOTU.
     STOP     RUN.
*----<< 売上データ >>--*
 TOKU-ERR               SECTION.
     USE AFTER     EXCEPTION PROCEDURE      TOKU.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### OSKT190 TOKU ERROR " TOKU-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     CLOSE    TOKU.
     CLOSE    HDENJNL.
     CLOSE    HKYOTU.
     STOP     RUN.
*----<< ワークＦ >>--*
 HKYOTU-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HKYOTU.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### OSKT190 HKYOTU  ERROR " KYO-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     CLOSE    TOKU.
     CLOSE    HDENJNL.
     CLOSE    HKYOTU.
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
     DISPLAY  "*** OSKT190 START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     I-O       HDENJNL.
     OPEN     I-O       ZNYUKDT.
     OPEN     I-O       ZNYUSDT.
*****OPEN     I-O       ZSGYODT.
     OPEN     INPUT     TOKU.
     OPEN     OUTPUT    HKYOTU.
*クリア
     INITIALIZE         FLAGS  WK-CNT.
*売上Ｆリード
     PERFORM       TOKU-RD-SEC.
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
*伝票Ｄ索引
*伝票区分の上１けたが　４　でないとき
     IF     TOKU-F02(1:1) NOT =  "4"
*入庫処理へ
            PERFORM              NYUK-SEC
*入庫データが存在しないとき　入出庫処理
            IF   INV-FLG  =      1
                 PERFORM              NYUS-SEC
            END-IF
                 GO TO  MAIN-READ
     END-IF.
*### 99/05/28 NAV START ###*  大阪売上の場合，特販部得意先へ
*****MOVE   TOKU-F06      TO     DEN-F24.
     EVALUATE   TOKU-F06
*        サンサンランド_
         WHEN   "27536580"
                 MOVE  "61100209"  TO  DEN-F24
*        トーク
         WHEN   "28514010"
                 MOVE  "61101060"  TO  DEN-F24
*        コーナン
         WHEN   "27532070"
                 MOVE  "61101100"  TO  DEN-F24
*        丸長商事_
         WHEN   "30583520"
                 MOVE  "61101130"  TO  DEN-F24
*        ニック産業_
         WHEN   "26569010"
                 MOVE  "61101140"  TO  DEN-F24
*        丸長商事_パワーセンターマルチョウ
         WHEN   "30583510"
                 MOVE  "61101240"  TO  DEN-F24
*        _キッコリー
         WHEN   "27527060"
                 MOVE  "61101320"  TO  DEN-F24
*        その他の取引先コードの場合
         WHEN   OTHER
                 MOVE   TOKU-F06   TO  DEN-F24
     END-EVALUATE.
*### 99/05/28 NAV END   ###*
     MOVE   TOKU-F03      TO     DEN-F23.
     MOVE   TOKU-F05      TO     DEN-F04.
     MOVE   TOKU-F02      TO     DEN-F051.
     MOVE   TOKU-F04      TO     DEN-F03.
     READ   HDENJNL
       INVALID
          DISPLAY  NC"伝票未登録＝" " "
                       TOKU-F06 " "  TOKU-F03 " "
                       TOKU-F05 " "  TOKU-F02 " "
                       TOKU-F04      UPON CONS
          ADD  1       TO  ERR-CNT
       NOT INVALID
          MOVE ZERO    TO  DEN-F277  DEN-F45
          MOVE 9       TO  DEN-F27C
          MOVE DEN-REC TO  KYO-REC
          REWRITE      DEN-REC
          WRITE        KYO-REC
          ADD  1       TO  DEN-CNT
     END-READ.
*
 MAIN-READ.
*売上Ｆリード
     PERFORM       TOKU-RD-SEC.
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*                                                              *
*--------------------------------------------------------------*
 NYUK-SEC               SECTION.
*
     MOVE     ZERO          TO   INV-FLG.
     MOVE     TOKU-F03(1:7) TO   NYU-F02(1:7).
     MOVE     TOKU-F03(8:2) TO   NYU-F03.
     MOVE     TOKU-F05      TO   NYU-F04.
     MOVE     TOKU-F04      TO   NYU-F05.
     READ     ZNYUKDT
              INVALID
                 MOVE    1  TO   INV-FLG
                 GO TO           NYUK-EXIT
     END-READ.
*
     MOVE     ZERO          TO   NYU-F28.
     REWRITE  NYU-REC.
 NYUK-EXIT.
     EXIT.
*--------------------------------------------------------------*
*                                                              *
*--------------------------------------------------------------*
 NYUS-SEC               SECTION.
*
     MOVE     ZERO          TO   INV-FLG.
     MOVE     TOKU-F03      TO   NYS-F02.
     MOVE     TOKU-F04      TO   NYS-F03.
     START    ZNYUSDT    KEY     IS  >=
                                 NYS-F02
                                 NYS-F03
              INVALID    KEY
                         GO TO   SGYO-RD.
*

 NYUS-RD.
     READ     ZNYUSDT    NEXT
              AT END     GO TO   SGYO-RD.
     DISPLAY  NYS-F02 UPON CONS.
     DISPLAY  NYS-F03 UPON CONS.
*
     IF       NYS-F02(1:7)   >   TOKU-F03(3:7)
              GO TO              SGYO-RD.
*
     IF       NYS-F03    NOT =   TOKU-F04
              GO TO              NYUS-RD.
*
     IF       NYS-F02(1:7)   =   TOKU-F03(3:7)
              MOVE     ZERO       TO      NYS-F14
              REWRITE  NYS-REC
              GO TO              NYUS-EXIT.
*
 SGYO-RD.
***  MOVE     TOKU-F03      TO   SGY-F01.
***  MOVE     TOKU-F04      TO   SGY-F02.
***  READ     ZSGYODT
****          INVALID
****             MOVE    1  TO   INV-FLG
****             GO TO           NYUS-EXIT
**** END-READ.
****
**** MOVE     ZERO          TO   SGY-F13.
*****REWRITE  SGY-REC.
 NYUS-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     CLOSE    HDENJNL ZNYUKDT ZNYUSDT.
     CLOSE    HKYOTU.
     CLOSE    TOKU.
*
     DISPLAY  NC"売上Ｄ　　　件数＝" TOKU-CNT UPON CONS.
     DISPLAY  NC"伝票Ｄ　更新件数＝" DEN-CNT  UPON CONS.
     DISPLAY  NC"伝票Ｄエラー件数＝" ERR-CNT  UPON CONS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** OSKT190 END *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4      売上Ｆリード                                *
*--------------------------------------------------------------*
 TOKU-RD-SEC            SECTION.
     READ   TOKU   AT  END
       MOVE   9    TO  END-FLG
       GO     TO       TOKU-RD-EXIT
     END-READ.
     ADD      1    TO  TOKU-CNT.
 TOKU-RD-EXIT.
     EXIT.

```
