****************************************************************
*新基幹システムへ移行 1999/10/16 T.TAKAHASHI                   *
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    業務名　　　　　　　：　販売管理システム　　　　　　　　　*
*    モジュール名　　　　：　売上Ｄ作成ＦＬＧゼロ更新　　　　　*
*    作成日／更新日　　　：　00/05/31                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　売上Ｄより伝票Ｄを索引し　　　　　*
*                        ：　伝票Ｄの売上Ｄ作成ＦＬＧを　　　　*
*                        ：　ゼロクリアする　　　　　　　　　　*
*                                                              *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            TSY8011B.
 AUTHOR.                T.A. => Y.Y.
 DATE-WRITTEN.          00/05/31.
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
     SELECT   NYKFILF   ASSIGN         DA-01-VI-NYKFILL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  NYK-F02
                                       NYK-F03
                                       NYK-F04
                                       NYK-F05
                        STATUS         NYKFILF-ST.
*----<<入出庫データ>>--*
     SELECT   NYSFILF   ASSIGN         DA-01-VI-NYSFILL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE DYNAMIC
                        RECORD    KEY  NYS-F02
                                       NYS-F03
                        STATUS         NYSFILF-ST.
*----<<作業データ>>--*
     SELECT   SGYFILF   ASSIGN         DA-01-VI-SGYFILL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  SGY-F01
                                       SGY-F02
                        STATUS         SGYFILF-ST.
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
*----<< 入庫データ >>--*
 FD  NYKFILF            LABEL     RECORD   IS   STANDARD.
     COPY     NYKFILF   OF        XFDLIB
              JOINING   NYK       PREFIX.
*----<< 入出庫データ >>--*
 FD  NYSFILF            LABEL     RECORD   IS   STANDARD.
     COPY     NYSFILF   OF        XFDLIB
              JOINING   NYS       PREFIX.
*----<< 作業データ >>--*
 FD  SGYFILF            LABEL     RECORD   IS   STANDARD.
     COPY     SGYFILF   OF        XFDLIB
              JOINING   SGY       PREFIX.
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
     03  NYK-CNT        PIC  9(07).
     03  SGY-CNT        PIC  9(07).
     03  NKD-CNT        PIC  9(07).
     03  NSD-CNT        PIC  9(07).
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  HDENJNL-ST        PIC  X(02).
 01  NYKFILF-ST        PIC  X(02).
 01  NYSFILF-ST        PIC  X(02).
 01  SGYFILF-ST        PIC  X(02).
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
*数値変換
 01  WK-HENKAN          PIC  X(02).
 01  FILLER             REDEFINES      WK-HENKAN.
     03  WK-HENKAN-1    PIC  9(02).
 01  WK-HENKAN2         PIC  X(07).
 01  FILLER             REDEFINES      WK-HENKAN2.
     03  WK-HENKAN-2    PIC  9(07).
 01  WK-HENKAN9         PIC  X(09).
 01  FILLER             REDEFINES      WK-HENKAN9.
     03  WK-HENKAN-9    PIC  9(09).
 01  WK-TOKU-F031       PIC  9(07)     VALUE  ZERO.
 01  WK-TOKU-F032       PIC  9(02)     VALUE  ZERO.
 01  WK-TOKU-F05        PIC  9(01)     VALUE  ZERO.
 01  WK-TOKU-F04        PIC  9(02)     VALUE  ZERO.
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
     DISPLAY  "### TSY8011B HDENJNL ERROR " HDENJNL-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     CLOSE    TOKU.
     CLOSE    HDENJNL  NYKFILF NYSFILF SGYFILF.
     CLOSE    HKYOTU.
     STOP     RUN.
*----<< 入庫データ >>--*
 NYKFILF-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      NYKFILF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### TSY8011B NYKFILF ERROR " NYKFILF-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     CLOSE    TOKU.
     CLOSE    HDENJNL  NYKFILF NYSFILF SGYFILF.
     CLOSE    HKYOTU.
     STOP     RUN.
*----<< 入出庫データ >>--*
 NYSFILF-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      NYSFILF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### TSY8011B NYSFILF ERROR " NYSFILF-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     CLOSE    TOKU.
     CLOSE    HDENJNL  NYKFILF NYSFILF SGYFILF.
     CLOSE    HKYOTU.
     STOP     RUN.
*----<< 作業データ >>--*
 SGYFILF-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SGYFILF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### TSY8011B SGYFILF ERROR " SGYFILF-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     CLOSE    TOKU.
     CLOSE    HDENJNL  NYKFILF NYSFILF SGYFILF.
     CLOSE    HKYOTU.
     STOP     RUN.
*----<< 売上データ >>--*
 TOKU-ERR               SECTION.
     USE AFTER     EXCEPTION PROCEDURE      TOKU.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### TSY8011B TOKU ERROR " TOKU-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     CLOSE    TOKU.
     CLOSE    HDENJNL  NYKFILF NYSFILF SGYFILF.
     CLOSE    HKYOTU.
     STOP     RUN.
*----<< ワークＦ >>--*
 HKYOTU-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HKYOTU.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### TSY8011B HKYOTU  ERROR " KYO-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     CLOSE    TOKU.
     CLOSE    HDENJNL  NYKFILF NYSFILF SGYFILF.
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
     DISPLAY  "*** TSY8011B START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     I-O       HDENJNL.
     OPEN     I-O       NYKFILF.
     OPEN     I-O       NYSFILF.
     OPEN     I-O       SGYFILF.
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
*作業実績処理へ
            IF  TOKU-F21 = "F1" OR "G1" OR "G3" OR "G4" OR "G5"
                        OR "G6" OR "H1" OR "H2" OR "H3" OR "H4"
                        OR "H5" OR "H6" OR "55" OR "58" OR "59"
                        PERFORM SGYO-SEC
                        GO TO  MAIN-READ
            END-IF
*入庫処理へ
            PERFORM              NYUK-SEC
*入庫データが存在しないとき　入出庫処理
            IF   INV-FLG  =      1
                 PERFORM              NYUS-SEC
            END-IF
            GO TO  MAIN-READ
     END-IF.
*### 03/06/13 NAV START ###*  大阪売上の場合，特販部得意先へ
     MOVE   TOKU-F06      TO     DEN-F24.
     EVALUATE   TOKU-F06
*        カーマ
         WHEN   "23521530"
                 MOVE  "61501310"  TO  DEN-F24
*        バロー
         WHEN   "21574020"
                 MOVE  "61501440"  TO  DEN-F24
*        ヒラサダ
         WHEN   "23177040"
                 MOVE  "61101050"  TO  DEN-F24
*        マンモス
         WHEN   "21565020"
                 MOVE  "61501280"  TO  DEN-F24
*        ハンズマン
         WHEN   "45174020"
                 MOVE  "61101190"  TO  DEN-F24
     END-EVALUATE.
     MOVE   TOKU-F03      TO     DEN-F23.
     MOVE   TOKU-F05      TO     DEN-F04.
     MOVE   TOKU-F02      TO     DEN-F051.
     MOVE   TOKU-F04      TO     DEN-F03.
     READ   HDENJNL
       INVALID
**        DISPLAY  NC"伝票未登録＝" " "
**                     TOKU-F06 " "  TOKU-F03 " "
**                     TOKU-F05 " "  TOKU-F02 " "
**                     TOKU-F04      UPON CONS
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
     MOVE     ZERO          TO   INV-FLG NYK-CNT.
     MOVE     TOKU-F03(1:7) TO   WK-HENKAN2.
     MOVE     WK-HENKAN-2   TO   NYK-F02  WK-TOKU-F031.
     MOVE     TOKU-F03(8:2) TO   WK-HENKAN.
     MOVE     WK-HENKAN-1   TO   NYK-F03  WK-TOKU-F032.
     MOVE     TOKU-F05      TO   NYK-F04  WK-TOKU-F05.
     MOVE     ZERO          TO   NYK-F05  WK-TOKU-F04.
*入庫ファイルスタート
     START  NYKFILF  KEY  IS  >=  NYK-F02 NYK-F03 NYK-F04 NYK-F05
            INVALID
            MOVE     1      TO   INV-FLG
            GO              TO   NYUK-EXIT
     END-START.
*入庫ファイル読込み
 NYUK-010.
     READ     NYKFILF
              NEXT  AT END
              GO            TO  NYUK-EXIT
     END-READ.
*同一対象判定
*****DISPLAY "F031:F02 = " WK-TOKU-F031 " : " NYK-F02 UPON CONS.
*****DISPLAY "F032:F03 = " WK-TOKU-F032 " : " NYK-F03 UPON CONS.
*****DISPLAY "F05 :F04 = " WK-TOKU-F05  " : " NYK-F04 UPON CONS.
     IF  WK-TOKU-F031  =  NYK-F02
     AND WK-TOKU-F032  =  NYK-F03
     AND WK-TOKU-F05   =  NYK-F04
*        計上済ＦＬＧ初期化
         MOVE     ZERO      TO   NYK-F31
         REWRITE  NYK-REC
         ADD      1         TO   NYK-CNT  NKD-CNT
         GO                 TO   NYUK-010
     ELSE
         IF       NYK-CNT  =  ZERO
                  MOVE  1   TO   INV-FLG
         END-IF
     END-IF.
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
     START    NYSFILF    KEY     IS  >=
                                 NYS-F02
                                 NYS-F03
              INVALID    KEY
                         GO TO   NYUS-EXIT.
*

 NYUS-RD.
     READ     NYSFILF    NEXT
              AT END     GO TO   NYUS-EXIT.
     DISPLAY  NYS-F02 UPON CONS.
     DISPLAY  NYS-F03 UPON CONS.
*
     IF       NYS-F02(1:7)   >   TOKU-F03(3:7)
              GO TO              NYUS-EXIT.
*
     IF       NYS-F03    NOT =   TOKU-F04
              GO TO              NYUS-RD.
*
     IF       NYS-F02(1:7)   =   TOKU-F03(3:7)
              MOVE     ZERO       TO      NYS-F14
              ADD      1          TO      NSD-CNT
              REWRITE  NYS-REC
              GO TO              NYUS-EXIT.
*
 NYUS-EXIT.
     EXIT.
*--------------------------------------------------------------*
*     作業データ読込み（計上Ｆクリア）                         *
*--------------------------------------------------------------*
 SGYO-SEC               SECTION.
*    作業データ読込み
     MOVE     TOKU-F03      TO   WK-HENKAN9.
     MOVE     WK-HENKAN-9   TO   SGY-F01.
     MOVE     TOKU-F04      TO   SGY-F02.
     READ     SGYFILF
              INVALID    GO TO   SGYO-EXIT
     END-READ.
*
     MOVE     ZERO          TO   SGY-F13.
     ADD      1             TO   SGY-CNT.
     REWRITE  SGY-REC.
*
 SGYO-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     CLOSE    HDENJNL NYKFILF NYSFILF SGYFILF.
     CLOSE    HKYOTU.
     CLOSE    TOKU.
*
     DISPLAY  NC"電算エラーＤ件数＝" TOKU-CNT UPON CONS.
     DISPLAY  NC"伝票Ｄ　更新件数＝" DEN-CNT  UPON CONS.
     DISPLAY  NC"作業Ｄ　更新件数＝" SGY-CNT  UPON CONS.
     DISPLAY  NC"入庫Ｄ　更新件数＝" NKD-CNT  UPON CONS.
     DISPLAY  NC"入出庫Ｄ更新件数＝" NSD-CNT  UPON CONS.
     DISPLAY  NC"伝票Ｄエラー件数＝" ERR-CNT  UPON CONS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** TSY8011B END *** "
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
