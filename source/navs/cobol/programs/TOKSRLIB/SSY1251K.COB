****************************************************************
*                                                              *
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    業務名　　　　　　　：　販売管理システム　　　　　　　　　*
*    モジュール名　　　　：　請求合計ファイル作成　　　　　　　*
*    作成日／更新日　　　：　92/12/03                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　条件Ｆの締め日と一致した取引先の　*
*                        ：　請求金額を集計して，請求合計Ｆに　*
*                        ：　出力する。　　　　　　　　　　　　*
*    更新日／更新者　　　：　11/10/07 /YOSHIDA.M               *
*    更新概要　　　　　　：　基幹サーバ統合                    *
*                                                              *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSY1251K.
 AUTHOR.                S.K  SANKYO.
 DATE-WRITTEN.          92/12/03.
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
*----<< 取引先マスタ >>--*
     SELECT   HTOKMS    ASSIGN         DA-01-VI-TOKMS2
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  TOK-F01
                        STATUS         TOK-ST.
*----<< 伝票データ >>--*
     SELECT   HDENJNL   ASSIGN         DA-01-VI-DENJNL5
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  DEN-F01   DEN-F23
                                       DEN-F04   DEN-F051
***2011.10.07 ST
                                       DEN-F07   DEN-F112
***2011.10.07 EN
                                       DEN-F03
                        STATUS         DEN-ST.
*----<< 条件ファイル >>--*
     SELECT   HJYOKEN   ASSIGN         DA-01-VI-JYOKEN1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  JYO-F01   JYO-F02
                        STATUS         JYO-ST.
*----<< 請求合計Ｆ >>--*
     SELECT   KOUSEIF   ASSIGN         DA-01-VI-KOUSEIL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  SEI-F02   SEI-F01
                                       SEI-F04   SEI-F03
                                       SEI-F06
                        STATUS         SEI-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 取引先マスタ >>--*
 FD  HTOKMS             LABEL RECORD   IS   STANDARD.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
*----<< 伝票データ >>--*
 FD  HDENJNL            LABEL     RECORD   IS   STANDARD.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   DEN       PREFIX.
*----<< 条件ファイル >>--*
 FD  HJYOKEN            LABEL RECORD   IS   STANDARD.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYO       PREFIX.
*----<< 請求合計Ｆ >>--*
 FD  KOUSEIF            LABEL RECORD   IS   STANDARD.
     COPY     KOUSEIF   OF        XFDLIB
              JOINING   SEI       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
   03  END-FLG          PIC  9(01).
   03  INV-FLG          PIC  9(01).
   03  KOUSEIF-INV-FLG  PIC  X(03)  VALUE  SPACE.
   03  KOSIN-FLG        PIC  X(02)  VALUE  SPACE.
 01  COUNTERS.
     03  TOK-CNT        PIC  9(06).
     03  DEN-CNT        PIC  9(06).
     03  WT-CNT         PIC  9(06).
     03  RW-CNT         PIC  9(06).
     03  NG-CNT         PIC  9(06).
     03  OUT-CNT        PIC  9(06).
 01  IDX.
     03  I              PIC  9(03).
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  TOK-ST             PIC  X(02).
 01  DEN-ST             PIC  X(02).
 01  JYO-ST             PIC  X(02).
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
*伝票番号編集
 01  WK-SEI-F05.
     03  WK-SEI-F05-1   PIC  X(01)  VALUE  SPACE.
     03  WK-SEI-F05-2   PIC  X(08)  VALUE  SPACE.
 01  WK-NOHIN-DATE      PIC  9(08)  VALUE  ZERO.
*
*----<< BREAK KEY >>--*
 01  BREAK-KEY.
     03  NEW.
         05  NEW-TOR                        PIC  9(08).
         05  NEW-DEN                        PIC  X(09).
     03  OLD.
         05  OLD-TOR                        PIC  9(08).
         05  OLD-DEN                        PIC  X(09).
*伝票退避ワーク
     COPY   KOUSEIF OF XFDLIB  JOINING   SWK  AS   PREFIX.
 01  DATE-AREA.
     03  WK-YS                    PIC  9(02)  VALUE  ZERO.
     03  WK-DATE.
         05  WK-Y                 PIC  9(02)  VALUE  ZERO.
         05  WK-M                 PIC  9(02)  VALUE  ZERO.
         05  WK-D                 PIC  9(02)  VALUE  ZERO.
 01  DATE-AREAR2       REDEFINES      DATE-AREA.
     03  SYS-DATE                 PIC  9(08).
 01  CHK-DATE-WORK.
     03  CHK-01              PIC  9(02).
     03  CHK-02              PIC  9(02).
     03  MATUBI              PIC  X(24)  VALUE
         "312831303130313130313031".
     03  FILLER              REDEFINES   MATUBI.
         05  WK-MATUBI       PIC  9(02)  OCCURS  12.
     03  WK-NOUHIN           PIC  9(08).
     03  FILLER              REDEFINES   WK-NOUHIN.
         05  WK-NOUHIN-YY    PIC  9(04).
         05  WK-NOUHIN-MM    PIC  9(02).
         05  WK-NOUHIN-DD    PIC  9(02).
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*
****************************************************************
 PROCEDURE              DIVISION.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 取引先マスタ >>--*
 HTOKMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTOKMS.
     ACCEPT   WK-DATE        FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SSY1251K HTOKMS ERROR " TOK-ST " "
              WK-Y "." WK-M "." WK-D " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 伝票データ >>--*
 HDENJNL-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HDENJNL.
     ACCEPT   WK-DATE        FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SSY1251K HDENJNL ERROR " DEN-ST " "
              WK-Y "." WK-M "." WK-D " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 条件ファイル >>--*
 HJYOKEN-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HJYOKEN.
     ACCEPT   WK-DATE        FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SSY1251K HJYOKEN ERROR " JYO-ST " "
              WK-Y "." WK-M "." WK-D " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 請求合計Ｆ >>--*
 KOUSEIF-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      KOUSEIF.
     ACCEPT   WK-DATE        FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY "SEI-F01 = " SEI-F01 UPON CONS.
     DISPLAY "SEI-F02 = " SEI-F02 UPON CONS.
     DISPLAY "SEI-F03 = " SEI-F03 UPON CONS.
     DISPLAY "SEI-F04 = " SEI-F04 UPON CONS.
     DISPLAY "SEI-F05 = " SEI-F05 UPON CONS.
     DISPLAY "SEI-F06 = " SEI-F06 UPON CONS.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SSY1251K KOUSEIF ERROR " SEI-ST " "
              WK-Y "." WK-M "." WK-D " "
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
     ACCEPT   WK-DATE        FROM DATE.
     MOVE     "3"                 TO   LINK-IN-KBN.
     MOVE     WK-DATE             TO   LINK-IN-YMD6.
     MOVE     ZERO                TO   LINK-IN-YMD8.
     MOVE     ZERO                TO   LINK-OUT-RET.
     MOVE     ZERO                TO   LINK-OUT-YMD.
     CALL     "SKYDTCKB"       USING   LINK-IN-KBN
                                       LINK-IN-YMD6
                                       LINK-IN-YMD8
                                       LINK-OUT-RET
                                       LINK-OUT-YMD.
     MOVE      LINK-OUT-YMD       TO   DATE-AREA.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSY1251K START *** "
              WK-Y "." WK-M "." WK-D " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     INPUT     HTOKMS.
     OPEN     I-O       HDENJNL.
     OPEN     INPUT     HJYOKEN.
     OPEN     I-O       KOUSEIF.
*----<< ﾜｰｸ ｼｮｷｾｯﾄ >>-*
     INITIALIZE         FLAGS.
     INITIALIZE         COUNTERS.
*
     MOVE     59        TO   JYO-F01.
     MOVE     SPACE     TO   JYO-F02.
     PERFORM  900-JYO-READ.
     IF       INV-FLG   =    0
              MOVE      JYO-F05   TO   SIME-DATE SSKTLSTD-DATE
     ELSE
              MOVE      1         TO   END-FLG
     END-IF.
*
     IF       END-FLG   =    1
              GO                  TO   100-INIT-RTN-EXIT
     END-IF.
*ブレイクキー初期化
     MOVE     LOW-VALUE      TO   BREAK-KEY.
*ワーク初期化
     MOVE     SPACE          TO   SWK-REC.
     INITIALIZE                   SWK-REC.
*コーナン取引先ＣＤセット
***2011.10.07 ST
     MOVE     SPACE          TO   DEN-REC.
     INITIALIZE                   DEN-REC.
***2011.10.07 EN
     MOVE     23631          TO   DEN-F01.
     INITIALIZE         DEN-F23.
     INITIALIZE         DEN-F04.
     INITIALIZE         DEN-F051.
     INITIALIZE         DEN-F03.
*売上ファイルスタート
     PERFORM  900-DEN-START-READ.
     MOVE     NEW            TO   OLD.
*
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
*
     PERFORM  210-SYUKEI
                   UNTIL     NEW       =    HIGH-VALUE.
*
     IF       SWK-F01        =    23631
*****AND      SWK-F07   NOT  =    ZERO
              PERFORM   KOUSEIF-READ-RTN
     END-IF.
*
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     CLOSE    HTOKMS.
     CLOSE    HDENJNL.
     CLOSE    HJYOKEN.
     CLOSE    KOUSEIF.
*
     DISPLAY  "# 売上ﾃﾞｰﾀ READ = " DEN-CNT " #" UPON CONS.
     DISPLAY  "# 請求ﾃﾞｰﾀ　CRT = " WT-CNT  " #" UPON CONS.
     DISPLAY  "# 請求ﾃﾞｰﾀ　RWT = " RW-CNT  " #" UPON CONS.
     DISPLAY  "# 請求ﾃﾞｰﾀ　NG  = " NG-CNT  " #" UPON CONS.
     ACCEPT   WK-DATE        FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSY1251K END *** "
              WK-Y "." WK-M "." WK-D " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3     ｼｭｳｹｲ                                        *
*--------------------------------------------------------------*
 210-SYUKEI             SECTION.
     IF       NEW  NOT  =    OLD
**************IF   SWK-F07   NOT  =    ZERO
                   PERFORM   KOUSEIF-READ-RTN
**************END-IF
              MOVE      SPACE     TO   SWK-REC
              INITIALIZE               SWK-REC
     END-IF.
*
     IF       DEN-F04   =    1    OR   3    OR   5
                        OR   7    OR   9
              MULTIPLY       -1   BY   DEN-F181
              MULTIPLY       -1   BY   DEN-F15
     END-IF.
     IF       DEN-F051  =    41   OR   42   OR   47   OR   48
              MULTIPLY       -1   BY   DEN-F181
              MULTIPLY       -1   BY   DEN-F15
     END-IF.
*取引先ＣＤ
     MOVE     DEN-F01        TO   SWK-F01.
*締日（画面入力項目）
*****MOVE     SSKTLSTD-DATE  TO   SWK-F02.
*店舗ＣＤ
     MOVE     DEN-F07        TO   SWK-F03.
*納品日（検収日）
     MOVE     DEN-F112       TO   SWK-F04  WK-NOUHIN.
*伝票区分
*****MOVE     DEN-F132       TO   SWK-F05.
     MOVE     DEN-F051       TO   SWK-F05.
*伝票番号
     MOVE     DEN-F02        TO   SWK-F06.
*請求金額
     ADD      DEN-F181       TO   SWK-F07.
*部門
     MOVE     DEN-F12        TO   SWK-F08.
*データ抽出日
     MOVE     SSKTLSTD-DATE  TO   SWK-F14.
*月末日取得
     DIVIDE   WK-NOUHIN-YY   BY   4
              GIVING    CHK-01    REMAINDER      CHK-02.
     IF       CHK-02    =    0
              MOVE      29        TO   WK-MATUBI (2)
     ELSE
              MOVE      28        TO   WK-MATUBI (2)
     END-IF.
*
     MOVE     WK-MATUBI(WK-NOUHIN-MM) TO  WK-NOUHIN-DD.
     MOVE     WK-NOUHIN           TO   SWK-F02.
*
     MOVE     NEW            TO   OLD.
     PERFORM  900-DEN-READ.
*
 210-SYUKEI-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  4      ｾｲｷｭｳ ﾃﾞｰﾀ ｼｭﾂﾘｮｸ                           *
*--------------------------------------------------------------*
 220-SEI-OUT            SECTION.
     WRITE    SEI-REC.
     ADD      1         TO   OUT-CNT.
 220-SEI-OUT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    条件ファイル　 READ                          *
*--------------------------------------------------------------*
 900-JYO-READ           SECTION.
     MOVE     0         TO   INV-FLG.
     READ     HJYOKEN   INVALID
              MOVE      1         TO   INV-FLG
     DISPLAY  "### SSY1251K HJYOKEN INVALID KEY=" JYO-F01 " ###"
                                       UPON CONS
     END-READ.
 900-JYO-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    取引先マスタ　 READ                          *
*--------------------------------------------------------------*
 900-TOK-READ           SECTION.
     READ     HTOKMS    AT   END
              MOVE      1         TO   END-FLG
              GO   TO   900-TOK-READ-EXIT
     END-READ.
*
     IF       TOK-F12 (1)    NOT  =    SIME-DD
     AND      TOK-F12 (2)    NOT  =    SIME-DD
     AND      TOK-F12 (3)    NOT  =    SIME-DD
              GO   TO   900-TOK-READ
     END-IF.
     ADD      1         TO   TOK-CNT.
 900-TOK-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    伝票ファイル　 START READ                    *
*--------------------------------------------------------------*
 900-DEN-START-READ     SECTION.
***2011.10.07 ST
***  START    HDENJNL   KEY  >=   DEN-F01   DEN-F23
***                               DEN-F04   DEN-F051
***                               DEN-F03
     START    HDENJNL   KEY  >=   DEN-F01   DEN-F23
                                  DEN-F04   DEN-F051
                                  DEN-F07   DEN-F112
                                  DEN-F03
***2011.10.07 EN
              INVALID   KEY
                        MOVE HIGH-VALUE     TO   NEW
     END-START.
     IF       NEW  NOT  =    HIGH-VALUE
              PERFORM   900-DEN-READ
     END-IF.
 900-DEN-START-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    伝票ファイル　 READ                          *
*--------------------------------------------------------------*
 900-DEN-READ           SECTION.
     READ     HDENJNL   AT   END
              MOVE      HIGH-VALUE     TO   NEW
              MOVE      1              TO   END-FLG
              GO   TO   900-DEN-READ-EXIT
     END-READ.
*
     ADD      1         TO   DEN-CNT.
*
     IF  DEN-CNT(4:3)  =  "000" OR "500"
         DISPLAY "# READ-CNT = " DEN-CNT ":" DEN-F112  UPON CONS
     END-IF.
*
 901-DEN.
*取引先ＣＤが異なる時
     IF       DEN-F01   NOT  =    23631
              MOVE      HIGH-VALUE     TO   NEW
              MOVE      1              TO   END-FLG
              GO   TO   900-DEN-READ-EXIT
     END-IF.
 902-DEN.
*納品日が下記以下は対象データとしない。
     MOVE     DEN-F112     TO    WK-NOHIN-DATE.
     IF       WK-NOHIN-DATE  <  20040901
              GO   TO   900-DEN-READ
     END-IF.
 903-DEN.
*請求ＦＬＧ
     IF       DEN-F261  NOT  =    0    AND  1
              GO   TO   900-DEN-READ
     END-IF.
 904-DEN.
*行番号＝９０
     IF       DEN-F03   =    90
              MOVE      1    TO   DEN-F261
              GO   TO   900-DEN-READ
     END-IF.
 905-DEN.
*行番号＝８０
     IF       DEN-F03   =    80
              MOVE      1    TO   DEN-F261
              GO   TO   900-DEN-READ
     END-IF.
 906-DEN.
*売上データ
     IF       DEN-F277  NOT  =    9
              GO   TO   900-DEN-READ
     END-IF.
 907-DEN.
*画面指定月末日
     IF       DEN-F112  >    SSKTLSTD-DATE
              GO   TO   900-DEN-READ
     END-IF.
 908-DEN.
*伝票区分チェック
*****IF       DEN-F051  =  40  OR  41
     IF       DEN-F051  =  40  OR  41  OR  42
              CONTINUE
     ELSE
              GO   TO   900-DEN-READ
     END-IF.
*請求確定チェック（確定済は対象としない）
     IF       DEN-F98   =    9
              GO   TO   900-DEN-READ
     END-IF.
*請求ＦＬＧセット
     MOVE     1         TO   DEN-F261   DEN-F98.
     PERFORM  900-DEN-REWRITE.
*
 909-DEN.
     IF       DEN-F133  =    9
              GO   TO   900-DEN-READ
     END-IF.
*
     MOVE     DEN-F01        TO   NEW-TOR.
     MOVE     DEN-F23        TO   NEW-DEN.
 900-DEN-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    伝票ファイル　 REWRITE                       *
*--------------------------------------------------------------*
 900-DEN-REWRITE        SECTION.
     REWRITE  DEN-REC.
 900-DEN-REWRITE-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    コーナン請求締日ファイル存在チェック         *
*--------------------------------------------------------------*
 KOUSEIF-READ-RTN       SECTION.
*
     MOVE     SWK-F02   TO   SEI-F02.
*****MOVE     SWK-F01   TO   SEI-F01.
     MOVE     2363      TO   SEI-F01.
     MOVE     SWK-F04   TO   SEI-F04.
     MOVE     SWK-F03   TO   SEI-F03.
     MOVE     SWK-F06   TO   SEI-F06.
     READ     KOUSEIF
              INVALID      MOVE  "INV"   TO   KOUSEIF-INV-FLG
              NOT  INVALID MOVE  SPACE   TO   KOUSEIF-INV-FLG
     END-READ.
*
     IF       KOUSEIF-INV-FLG = "INV"
              MOVE   "WT"    TO   KOSIN-FLG
     ELSE
              IF  SEI-F09  =  "1"  OR  SEI-F11  =  "1"
                  MOVE  "NG" TO   KOSIN-FLG
              ELSE
                  MOVE  "RW" TO   KOSIN-FLG
              END-IF
     END-IF.
*
     EVALUATE  KOSIN-FLG
         WHEN  "WT"
               MOVE   SPACE  TO   SEI-REC
               INITIALIZE         SEI-REC
               MOVE  SWK-REC TO   SEI-REC
               MOVE  2363    TO   SEI-F01
               WRITE SEI-REC
               ADD   1       TO   WT-CNT
         WHEN  "RW"
               DELETE  KOUSEIF
               MOVE   SPACE  TO   SEI-REC
               INITIALIZE         SEI-REC
               MOVE  SWK-REC TO   SEI-REC
               MOVE  2363    TO   SEI-F01
               WRITE SEI-REC
               ADD   1       TO   RW-CNT
         WHEN  "NG"
               ADD   1       TO   NG-CNT
     END-EVALUATE.

*
 KOUSEIF-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
