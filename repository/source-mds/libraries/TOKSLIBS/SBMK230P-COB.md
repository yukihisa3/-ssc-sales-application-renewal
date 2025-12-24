# SBMK230P

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SBMK230P.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    業務名　　　　　　　：　販売管理システム　　　　　　　　　*
*    モジュール名　　　　：　流通ＢＭＳ不照合リスト発行コメリ　*
*    作成日／更新日　　　：　13/03/18                          *
*    作成者／更新者　　　：　ＮＡＶ三浦　　　　　　　　　　　　*
*    処理概要　　　　　　：　支払合計ファイルと請求合計ファイル*
*                        ：　を比較して不照合リストを作成する。*
*                            指定取引先コードのみ照合対象とする*
*    更新日／更新者　　　：　13/07/02 INOUE                    *
*    更新内容　　　　　　：　店舗グループインジケート処理に　　*
*                            誤りあり→修正　　　　　　　　　　*
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SBMK230P.
 AUTHOR.                NAV.
 DATE-WRITTEN.          13/03/14.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM.
 OBJECT-COMPUTER.       FACOM.
 SPECIAL-NAMES.
         YA        IS   YA
         YB        IS   PITCH-1-5
         YB-21     IS   BAIKAKU-1-5
         YA-21     IS   BAIKAKU
*        STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< 支払ワークファイル >>--*
     SELECT   BMSSIHW   ASSIGN    TO     DA-01-VI-BMSSI2W2
                        ORGANIZATION     INDEXED
                        ACCESS    MODE   SEQUENTIAL
                        RECORD    KEY    SIH-F013
                                         SIH-F405
                                         SIH-F402
                        STATUS           SIH-ST.
*----<< 請求合計Ｆ >>--*
     SELECT   HSEIGKF   ASSIGN    TO     DA-01-VI-SETGKFK3
                        ORGANIZATION     INDEXED
                        ACCESS    MODE   SEQUENTIAL
                        RECORD    KEY    SEI-F01 SEI-F03 SEI-F05
                        STATUS           SEI-ST.
*----<< 店舗マスタ >>--*
     SELECT   HTENMS    ASSIGN           DA-01-VI-TENMS1
                        ORGANIZATION     INDEXED
                        ACCESS    MODE   RANDOM
                        RECORD    KEY    TEN-F52   TEN-F011
                        STATUS           TEN-ST.
*----<< 取引先マスタ >>--*
     SELECT   HTOKMS    ASSIGN    TO     DA-01-VI-TOKMS2
                        ORGANIZATION     INDEXED
                        ACCESS    MODE   RANDOM
                        RECORD    KEY    TOK-F01
                        STATUS    TOK-ST.
*----<< プリンタ >>-*
     SELECT   PRTF      ASSIGN    TO     LP-04.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 支払ワークファイル >>--*
 FD  BMSSIHW            LABEL RECORD   IS   STANDARD
                        BLOCK CONTAINS 20   RECORDS.
*    COPY     BMSSIHW.
     COPY     BMSSIHW   OF        XFDLIB
              JOINING   SIH       PREFIX.
*----<< 請求合計Ｆ >>--*
 FD  HSEIGKF            LABEL RECORD   IS   STANDARD.
*    COPY     SETGKFA.
     COPY     SETGKFP   OF        XFDLIB
              JOINING   SEI       PREFIX.
*----<< 店舗マスタ >>XT--*
 FD  HTENMS             LABEL RECORD   IS   STANDARD.
*    COPY     HTENMS.
     COPY     HTENMS    OF        XFDLIB
              JOINING   TEN       PREFIX.
*取引先マスタ
 FD  HTOKMS.
*             COPY      HTOKMS.
              COPY      HTOKMS    OF   XFDLIB
              JOINING   TOK       AS   PREFIX.
*----<< プリンタ >>-*
 FD  PRTF               LABEL RECORD   IS   OMITTED.
 01  PRT-REC            PIC  X(200).
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  END-FLG1       PIC  X(03).
     03  END-FLG2       PIC  X(03).
     03  INV-FLG1       PIC  9(01).
     03  INV-FLG2       PIC  9(01).
 01  TEN-FLG            PIC  9(01)  VALUE 0.
 01  COUNTERS.
     03  LINE-CNT       PIC  9(03).
     03  PAGE-CNT       PIC  9(03).
*
     03  CNT-SEI        PIC  9(06).
     03  CNT-SIH        PIC  9(06).
     03  CNT-UNMACTH    PIC  9(06).
     03  CNT-MACTH      PIC  9(06).
     03  CNT-RDSEI      PIC  9(06).
     03  CNT-RDSIH      PIC  9(06).
 01  IDX.
     03  I              PIC  9(03).
*ワーク支払金額
 01  WK-KINGAK          PIC S9(08)  VALUE  ZERO.
*ワーク請求金額
 01  WK-SEIGAK          PIC S9(08)  VALUE  ZERO.
*ワーク取引先コード
 01  WK-TOKCD           PIC  9(08)  VALUE  ZERO.
*ワーク対象期間終了
 01  WK-SIMEBI          PIC  9(08)  VALUE  ZERO.
 01  WK-TENCDR.
     03  WK-TENCD       PIC  9(04).
 01  WK-TORCDR.
     03  WK-TORCD       PIC  9(09).
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  SIH-ST             PIC  X(02).
 01  SEI-ST             PIC  X(02).
 01  TEN-ST             PIC  X(02).
 01  TOK-ST             PIC  X(02).
*
*日付／時刻
 01  TIME-AREA.
     03  WK-TIME        PIC  9(08)  VALUE  ZERO.
 01  DATE-AREA.
     03  WK-YS          PIC  9(02)  VALUE  ZERO.
     03  WK-DATE.
         05  WK-Y       PIC  9(02)  VALUE  ZERO.
         05  WK-M       PIC  9(02)  VALUE  ZERO.
         05  WK-D       PIC  9(02)  VALUE  ZERO.
 01  DATE-AREAR2        REDEFINES      DATE-AREA.
     03  SYS-DATE1      PIC  9(08).
*
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN        PIC  X(01).
 01  LINK-IN-YMD6       PIC  9(06).
 01  LINK-IN-YMD8       PIC  9(08).
 01  LINK-OUT-RET       PIC  X(01).
 01  LINK-OUT-YMD       PIC  9(08).
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
 01  SEI-KEY.
     03  SEI-KEY1       PIC  9(04).
     03  SEI-KEY2       PIC  9(09).
*
 01  SIH-KEY.
     03  SIH-KEY1       PIC  9(04).
     03  SIH-KEY2       PIC  9(09).
*
 01  NEW-TENCD          PIC  X(04)  VALUE  SPACE.
 01  OLD-TENCD          PIC  X(04)  VALUE  SPACE.
*--<< ﾌﾟﾘﾝﾄ AREA >>-*
 01  HEAD01.
     03  FILLER         CHARACTER TYPE BAIKAKU-1-5.
         05  FILLER     PIC  X(01)  VALUE  SPACE.
         05  FILLER     PIC  X(08)  VALUE "SBMK230P".
         05  FILLER     PIC  X(37)  VALUE  SPACE.
         05  FILLER     PIC  N(14)  VALUE
                        NC"＜　　　不照合リスト　　　＞".
     03  FILLER         CHARACTER TYPE MODE-2.
         05  FILLER     PIC  X(25)  VALUE  SPACE.
         05  HD-011     PIC  ZZZ9.
         05  FILLER     PIC  X(01)  VALUE  "/".
         05  HD-012     PIC  Z9.
         05  FILLER     PIC  X(01)  VALUE  "/".
         05  HD-013     PIC  Z9.
         05  FILLER     PIC  X(02)  VALUE  SPACE.
         05  HD-02      PIC  ZZ9.
         05  FILLER     PIC  N(01)  VALUE  NC"頁".
****  見出し行２***
 01  HEAD02.
     03  FILLER         PIC  X(01)  VALUE  SPACE.
     03  FILLER         PIC  N(07)
         CHARACTER  TYPE  IS  YA    VALUE
         NC"照合請求締日：".
     03  SIMEBI         PIC  9999/99/99.
*
****  見出し行３***
 01  HEAD03.
     03  FILLER         PIC  X(07)  VALUE  SPACE.
     03  FILLER         PIC  N(04)
         CHARACTER  TYPE  IS  YA    VALUE
         NC"取引先：".
     03  TORICD         PIC  9(08).
     03  FILLER         PIC  X(01)  VALUE  SPACE.
     03  TORINAM        PIC  N(15)
         CHARACTER  TYPE  IS  YA.
*
 01  HEAD04         CHARACTER TYPE  YA.
     03  FILLER         PIC  X(01)  VALUE  SPACE.
     03  FILLER         PIC  N(04)  VALUE  NC"店舗情報".
     03  FILLER         PIC  X(20)  VALUE  SPACE.
     03  FILLER         PIC  N(04)  VALUE  NC"伝票区分".
     03  FILLER         PIC  X(09)  VALUE  SPACE.
     03  FILLER         PIC  N(04)  VALUE  NC"伝票番号".
     03  FILLER         PIC  X(05)  VALUE  SPACE.
     03  FILLER         PIC  N(04)  VALUE  NC"請求金額".
     03  FILLER         PIC  X(04)  VALUE  SPACE.
     03  FILLER         PIC  N(04)  VALUE  NC"支払金額".
     03  FILLER         PIC  X(02)  VALUE  SPACE.
     03  FILLER         PIC  N(08)  VALUE
                                    NC"レコード区分内容".
     03  FILLER         PIC  X(06)  VALUE  SPACE.
     03  FILLER         PIC  N(05)  VALUE  NC"データ内容".
****  線１　　　　　 ****
 01  HASEN-1.
     03  FILLER         PIC  X(136) VALUE  ALL "=".
*
 01  MEIS01.
     03  FILLER         PIC  X(01)  VALUE  SPACE.
     03  TENCD          PIC  ZZZZ9.
     03  FILLER         PIC  X(01)  VALUE  SPACE.
     03  TENNM          PIC  N(10)
                        CHARACTER   TYPE  IS  YA.
     03  FILLER         PIC  X(02)  VALUE  SPACE.
     03  DENKBN         PIC  X(04).
     03  FILLER         PIC  X(01)  VALUE  SPACE.
     03  DENKBNM        PIC  N(05)
                        CHARACTER   TYPE  IS  YA.
     03  FILLER         PIC  X(02)  VALUE  SPACE.
     03  DENNO          PIC  9(09).
     03  FILLER         PIC  X(01)  VALUE  SPACE.
     03  SEIKYU         PIC  ---,---,--9.
     03  FILLER         PIC  X(01)  VALUE  SPACE.
     03  SIHARAI        PIC  ---,---,--9.
     03  FILLER         PIC  X(02)  VALUE  SPACE.
     03  RNAIYO         PIC  N(10)
                        CHARACTER   TYPE  IS  YA.
     03  FILLER         PIC  X(02)  VALUE  SPACE.
     03  DNAIYO         PIC  N(10)
                        CHARACTER   TYPE  IS  YA.
*
 01  TAIL01         CHARACTER TYPE  YA.
     03  FILLER         PIC  X(02)   VALUE  SPACE.
     03  FILLER         PIC  N(07)   VALUE  NC"＜不照合情報＞".
     03  FILLER         PIC  X(02)   VALUE  SPACE.
     03  FILLER         PIC  N(07)   VALUE  NC"請求データ無：".
     03  TA-01          PIC  ---,--9.
     03  FILLER         PIC  X(01)   VALUE  SPACE.
     03  FILLER         PIC  N(07)   VALUE  NC"支払データ無：".
     03  FILLER         PIC  X(01)   VALUE  SPACE.
     03  TA-02          PIC  ---,--9.
     03  FILLER         PIC  X(01)   VALUE  SPACE.
     03  FILLER         PIC  N(05)   VALUE  NC"金額違い：".
     03  FILLER         PIC  X(01)   VALUE  SPACE.
     03  TA-03          PIC  ---,--9.
     03  FILLER         PIC  X(01)   VALUE  SPACE.
     03  FILLER         PIC  N(05)   VALUE  NC"照合件数：".
     03  FILLER         PIC  X(01)   VALUE  SPACE.
     03  TA-04          PIC  ---,--9.
*
 LINKAGE                SECTION.
 01  PARA-TOKCD     PIC   9(08).
 01  PARA-SHIME     PIC   9(08).
****************************************************************
 PROCEDURE              DIVISION USING  PARA-TOKCD PARA-SHIME.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 支払合計ファイル >>--*
 SIH-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      BMSSIHW.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SBMK230P BMSSIHW ERROR " SIH-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
**** CLOSE    BMSSIHW   HSEIGKF    HTENMS    PRTF.
     STOP     RUN.
*----<< 請求合計Ｆ >>--*
 HSEIGKF-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HSEIGKF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SBMK230P HSEIGKF ERROR " SEI-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
**** CLOSE    BMSSIHW   HSEIGKF    HTENMS    PRTF.
     STOP     RUN.
*----<< 店舗マスタ >>--*
 HTENMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTENMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SBMK230P HTENMS ERROR " TEN-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
*----<< 取引先マスタ >>--*
 TOK-ERR-EXC                 SECTION.
     USE      AFTER          EXCEPTION PROCEDURE HTOKMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SBMK230P HTOKMS ERROR " TOK-ST " "
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
     PERFORM  200-MAIN-RTN
              UNTIL     END-FLG1 = "END"  AND
                        END-FLG2 = "END".
     PERFORM  300-END-RTN.
     STOP RUN.
 000-PROG-CNTL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｼｮｷ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 100-INIT-RTN           SECTION.
**   DISPLAY "100-INIT-RTN"  UPON  CONS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SBMK230P START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
*システム日付・時刻の取得
     ACCEPT   WK-DATE           FROM   DATE.
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
*システム時刻取得
     ACCEPT    WK-TIME          FROM   TIME.
*
     OPEN     INPUT     BMSSIHW.
     OPEN     INPUT     HSEIGKF.
     OPEN     INPUT     HTENMS.
     OPEN     INPUT     HTOKMS.
     OPEN     OUTPUT    PRTF.
*----<< ﾜｰｸ ｼｮｷｾｯﾄ >>-*
     INITIALIZE         FLAGS.
     INITIALIZE         COUNTERS.
     MOVE     99             TO   LINE-CNT.
     MOVE     1              TO   INV-FLG1
                                  INV-FLG2.
     INITIALIZE         SIH-KEY
                        SEI-KEY.
**   DISPLAY "900-SIH-RTN"  UPON  CONS.
*    支払ワーク読込み
     PERFORM  900-SIH-READ  UNTIL INV-FLG1  = 0     OR
                                  END-FLG1  = "END".
     IF    END-FLG1     =   SPACE
*         取引先コード退避
           MOVE     SIH-F013     TO   WK-TOKCD
*          対象期間終了退避
           MOVE     SIH-F308     TO   WK-SIMEBI
     END-IF.
*    請求合計ワーク読込み
     PERFORM  SEI-START-SEC.
     PERFORM  900-SEI-READ  UNTIL INV-FLG2  = 0      OR
                                  END-FLG2  = "END".
     IF    END-FLG2     =   SPACE
*         取引先コード退避
           MOVE     SEI-F01      TO   WK-TOKCD
*          対象期間終了退避
*          MOVE     SEI-F02      TO   WK-SIMEBI
           COMPUTE  WK-SIMEBI  =  20000000 +  SEI-F02
     END-IF.
*    DISPLAY "WK-SIMEBI   : "  WK-SIMEBI UPON  CONS.
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
**   DISPLAY "200-MAIN-RTN"  UPON  CONS.
*
     IF       SIH-KEY        =    SEI-KEY
*↓2013/07/02
              IF  SEI-KEY1   NOT  =    OLD-TENCD
                  MOVE     0      TO   TEN-FLG
              END-IF
*↑2013/07/02
              PERFORM  210-MEI-RTN
*↓2013/07/02
              MOVE     SEI-KEY    TO   OLD-TENCD
*↑2013/07/02
              MOVE     1          TO   INV-FLG1
              PERFORM  900-SIH-READ  UNTIL INV-FLG1 =  0  OR
                                           END-FLG1 = "END"
              MOVE     1          TO   INV-FLG2
              PERFORM  900-SEI-READ  UNTIL INV-FLG2 =  0  OR
                                           END-FLG2 = "END"
     ELSE
         IF   SIH-KEY        >    SEI-KEY
***           DISPLAY SIH-KEY1 " : " SEI-KEY1 UPON CONS
              IF  SEI-KEY1   NOT  =    OLD-TENCD
                  MOVE     0      TO   TEN-FLG
              END-IF
              PERFORM  230-SIH-RTN
              MOVE     1          TO   INV-FLG2
              MOVE     SEI-KEY    TO   OLD-TENCD
              PERFORM  900-SEI-READ  UNTIL INV-FLG2 =  0  OR
                                           END-FLG2 = "END"
         ELSE
***           DISPLAY SIH-KEY1 " : " SEI-KEY1 UPON CONS
              IF  SIH-KEY1    NOT  =   OLD-TENCD
                  MOVE     0      TO   TEN-FLG
              END-IF
              PERFORM  220-SEI-RTN
              MOVE     1          TO   INV-FLG1
              MOVE     SIH-KEY    TO   OLD-TENCD
              PERFORM  900-SIH-READ  UNTIL INV-FLG1 =  0  OR
                                           END-FLG1 = "END"
         END-IF
     END-IF.
*
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      【金額不一致確認処理】　　　　　　　　　　　*
*--------------------------------------------------------------*
 210-MEI-RTN           SECTION.
**   DISPLAY "210-MEI-RTN"  UPON  CONS.
     MOVE      SIH-F419           TO    WK-KINGAK.
     IF        SIH-F420       =    "-"
               COMPUTE  WK-KINGAK   =   WK-KINGAK * -1
     END-IF.
     MOVE      SEI-F06            TO    WK-SEIGAK.
     IF        WK-KINGAK      =     WK-SEIGAK
               ADD    1           TO        CNT-MACTH
               GO                 TO        210-MEI-RTN-EXIT
     END-IF.
*
     IF        LINE-CNT   >    50
               PERFORM    210-HEAD-PRINT
     END-IF.
*
     MOVE      SPACE               TO       MEIS01.
     IF        TEN-FLG              =       0
               MOVE      SEI-F03    TO      TENCD
               MOVE      SEI-F03    TO      TEN-F011
               MOVE      SEI-F01    TO      TEN-F52
               PERFORM   900-TEN-READ
               MOVE      TEN-F03    TO      TENNM
               MOVE      1          TO      TEN-FLG
     END-IF.
*    伝票区分
     MOVE      SEI-F07            TO        DENKBN.
     EVALUATE   SEI-F07
         WHEN  "40"
                MOVE  NC"売上"    TO        DENKBNM
         WHEN  "41"
                MOVE  NC"返品"    TO        DENKBNM
         WHEN  "42"
                MOVE  NC"値引"    TO        DENKBNM
         WHEN   OTHER
                MOVE  ALL NC"＊"  TO        DENKBNM
     END-EVALUATE.
*
     MOVE      SEI-F05            TO        DENNO.
     MOVE      WK-SEIGAK          TO        SEIKYU.
     MOVE      WK-KINGAK          TO        SIHARAI.
     MOVE      NC"請求ＮＧデータ" TO        RNAIYO.
     MOVE      NC"金額不一致"     TO        DNAIYO.
*
     WRITE     PRT-REC   FROM MEIS01        AFTER     1.
     ADD       1                  TO        LINE-CNT.
     ADD       1                  TO        CNT-UNMACTH.
*
 210-MEI-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      【請求データ無処理】　　　　　　　　　　　　*
*--------------------------------------------------------------*
 220-SEI-RTN           SECTION.
**   DISPLAY "220-SEI-RTN"  UPON  CONS.
     IF        LINE-CNT  >    50
               PERFORM   210-HEAD-PRINT
     END-IF.
     MOVE      SPACE              TO        MEIS01.
     IF        TEN-FLG            =         0
               MOVE      SIH-F405(1:4)      TO     WK-TENCDR
               MOVE      WK-TENCD           TO        TENCD
               MOVE      SIH-F405(1:4)      TO     WK-TENCDR
               MOVE      WK-TENCD           TO        TEN-F011
               MOVE      SIH-F013           TO        TEN-F52
               PERFORM   900-TEN-READ
               MOVE      TEN-F03            TO        TENNM
               MOVE      1                  TO        TEN-FLG
     END-IF.
*
     MOVE      SIH-F428           TO        DENKBN.
     EVALUATE  SIH-F428
         WHEN "1001"
               MOVE    NC"仕入"   TO        DENKBNM
         WHEN "1002"
               MOVE    NC"返品"   TO        DENKBNM
         WHEN "1004"
               MOVE    NC"値引"   TO        DENKBNM
         WHEN "1005"
               MOVE    NC"委託"   TO        DENKBNM
         WHEN  OTHER
               MOVE    ALL NC"＊" TO        DENKBNM
     END-EVALUATE.
*
     MOVE      SIH-F402(1:9)      TO        DENNO.
     MOVE      SIH-F419           TO        WK-KINGAK.
     IF        SIH-F420       =    "-"
               COMPUTE  WK-KINGAK   =   WK-KINGAK * -1
     END-IF.
     MOVE      WK-KINGAK          TO        SIHARAI.
     MOVE      ZERO               TO        SEIKYU.
     MOVE      NC"請求データなし" TO        RNAIYO.
     MOVE      SPACE              TO        DNAIYO.
*
     WRITE     PRT-REC   FROM MEIS01        AFTER     1.
     ADD       1                  TO        LINE-CNT.
     ADD       1                  TO        CNT-SEI.
*
 220-SEI-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      【支払データ無処理】　　　　　　　　　　　　*
*--------------------------------------------------------------*
 230-SIH-RTN           SECTION.
**   DISPLAY "230-SIH-RTN"  UPON  CONS.
*
 230-SIH-010.
     IF        LINE-CNT  >    50
               PERFORM   210-HEAD-PRINT
     END-IF.
*
 230-SIH-020.
     MOVE      SPACE              TO        MEIS01.
     IF        TEN-FLG            =         0
               MOVE      SEI-F03            TO    TENCD
               MOVE      SEI-F03            TO    TEN-F011
               MOVE      SEI-F01            TO    TEN-F52
               PERFORM   900-TEN-READ
               MOVE      TEN-F03            TO    TENNM
               MOVE      1                  TO    TEN-FLG
     END-IF.
 230-SIH-030.
*    伝票区分
     MOVE    SEI-F07              TO        DENKBN.
     EVALUATE   SEI-F07
         WHEN  "40"
                MOVE  NC"売上"    TO        DENKBNM
         WHEN  "41"
                MOVE  NC"返品"    TO        DENKBNM
         WHEN  "42"
                MOVE  NC"値引"    TO        DENKBNM
         WHEN   OTHER
                MOVE  ALL NC"＊"  TO        DENKBNM
     END-EVALUATE.
*
 230-SIH-040.
     MOVE      SEI-F05            TO        DENNO.
     MOVE      SEI-F06            TO    WK-SEIGAK.
     MOVE      WK-SEIGAK          TO        SEIKYU.
     MOVE      ZERO               TO        SIHARAI.
     MOVE      NC"支払データ無"   TO        RNAIYO.
     MOVE      SPACE              TO        DNAIYO.
*
 230-SIH-050.
     WRITE     PRT-REC   FROM MEIS01        AFTER     1.
     ADD       1                  TO        LINE-CNT.
     ADD       1                  TO        CNT-SIH.
*
 230-SIH-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
**   DISPLAY "300-END-RTN"  UPON  CONS.
*    テイル印刷
     IF    PAGE-CNT     >     0
           PERFORM  TAIL-RTN
     END-IF.
*
     CLOSE    BMSSIHW.
     CLOSE    HSEIGKF.
     CLOSE    HTENMS.
     CLOSE    HTOKMS.
     CLOSE    PRTF.
*
     DISPLAY "支払ワーク読込件数："  CNT-RDSIH    UPON  CONS.
     DISPLAY "支払データ無件数　："  CNT-SIH      UPON  CONS.
     DISPLAY "金額アンマッチ件数："  CNT-UNMACTH  UPON  CONS.
     DISPLAY "請求合計読込件数　："  CNT-RDSEI    UPON  CONS.
     DISPLAY "請求データ無件数　："  CNT-SEI      UPON  CONS.
     DISPLAY "照合件数　　　　　："  CNT-MACTH    UPON  CONS.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SBMK230P END *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
*****ACCEPT   SIH-ST  FROM   CONS.
*
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL 4       ﾀｲﾄﾙ ﾌﾟﾘﾝﾄ ｼｮﾘ                              *
*--------------------------------------------------------------*
 210-HEAD-PRINT         SECTION.
**   DISPLAY "210-HEAD-PRINT"  UPON  CONS.
     IF       PAGE-CNT  >    0
              MOVE    SPACE       TO   PRT-REC
              WRITE   PRT-REC     AFTER  PAGE
     END-IF.
*
     ADD      1                   TO   PAGE-CNT.
     MOVE     LINK-OUT-YMD(1:4)   TO   HD-011.
     MOVE     SYS-MM              TO   HD-012.
     MOVE     SYS-DD              TO   HD-013.
     MOVE     PAGE-CNT            TO   HD-02.
*取引先
     MOVE     WK-TOKCD            TO   TORICD.
     MOVE     WK-TOKCD            TO   TOK-F01.
     PERFORM  900-TOK-READ.
     MOVE     TOK-F02             TO   TORINAM.
*照合請求締日
     MOVE     WK-SIMEBI           TO   SIMEBI.
*
     WRITE    PRT-REC   FROM      HEAD01    AFTER     1.
     WRITE    PRT-REC   FROM      HEAD02    AFTER     2.
     WRITE    PRT-REC   FROM      HEAD03    AFTER     1.
     WRITE    PRT-REC   FROM      HASEN-1   AFTER     1.
     WRITE    PRT-REC   FROM      HEAD04    AFTER     1.
     WRITE    PRT-REC   FROM      HASEN-1   AFTER     1.
     MOVE     8                   TO   LINE-CNT.
     MOVE     0                   TO   TEN-FLG.
 210-HEAD-PRINT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL 4       テイル部印刷　　　　　　　　　　　　　　　　*
*--------------------------------------------------------------*
 TAIL-RTN               SECTION.
**   DISPLAY "TAIL-RTN"  UPON  CONS.
     IF       LINE-CNT  >    50
              PERFORM   210-HEAD-PRINT
     END-IF.
*
     MOVE      CNT-SEI            TO        TA-01.
     MOVE      CNT-SIH            TO        TA-02.
     MOVE      CNT-UNMACTH        TO        TA-03.
     MOVE      CNT-MACTH          TO        TA-04.
*
     WRITE    PRT-REC   FROM      TAIL01    AFTER     2.
 TAIL-EXIT.
     EXIT.
****************************************************************
*    請求合計ファイルスタート
****************************************************************
 SEI-START-SEC                SECTION.
*
     MOVE     SPACE               TO   SEI-REC.
     INITIALIZE                        SEI-REC.
*
     MOVE     PARA-TOKCD          TO   SEI-F01.
*
     START  HSEIGKF  KEY    >=  SEI-F01 SEI-F03 SEI-F05
            INVALID  KEY
            DISPLAY "SINVALID KEY"   UPON  CONS
            MOVE    "END"         TO   END-FLG2
            GO TO SEI-START-EXIT
     END-START.
*
 SEI-START-EXIT.
     EXIT.
*
*--------------------------------------------------------------*
*    LEVEL ALL    請求合計Ｆ　　 READ                          *
*--------------------------------------------------------------*
 900-SEI-READ           SECTION.
**   DISPLAY "900-SEI-READ"  UPON  CONS.
     READ     HSEIGKF  NEXT  AT   END
              MOVE     "END"      TO   END-FLG2
              MOVE    HIGH-VALUE  TO   SEI-KEY
              GO   TO   900-SEI-READ-EXIT
     END-READ.
*
**  抽出条件判定
     IF   SEI-F01  >  PARA-TOKCD
          MOVE   "END"   TO   END-FLG2
              MOVE    HIGH-VALUE  TO   SEI-KEY
              GO   TO   900-SEI-READ-EXIT
     END-IF.
*対象期間終了
     COMPUTE  WK-SIMEBI  =  20000000 +  SEI-F02
     IF  WK-SIMEBI NOT =  PARA-SHIME
          MOVE   "END"   TO   END-FLG2
              MOVE    HIGH-VALUE  TO   SEI-KEY
              GO   TO   900-SEI-READ-EXIT
     END-IF.
     ADD      1                   TO   CNT-RDSEI.
     MOVE     SEI-F03             TO   SEI-KEY1.
     MOVE     SEI-F05             TO   SEI-KEY2.
     MOVE     0                   TO   INV-FLG2.
*
 900-SEI-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    店舗マスタ　　READ                           *
*--------------------------------------------------------------*
 900-TEN-READ           SECTION.
*    DISPLAY "900-TEN-READ"  UPON  CONS.
*    DISPLAY TEN-F011 " : " TEN-F52 UPON CONS.
     READ     HTENMS    INVALID
              MOVE      ALL NC"＊" TO   TEN-F03
     END-READ.
 900-TEN-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    取引先マスタ　READ                           *
*--------------------------------------------------------------*
 900-TOK-READ           SECTION.
**   DISPLAY "900-TOK-READ"  UPON  CONS.
     READ     HTOKMS    INVALID
              MOVE      ALL NC"＊" TO   TOK-F02
     END-READ.
 900-TOK-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    支払合計ファイル　 READ                      *
*--------------------------------------------------------------*
 900-SIH-READ           SECTION.
**   DISPLAY "900-SIH-READ"  UPON  CONS.
     READ     BMSSIHW   AT   END
              MOVE     "END"           TO   END-FLG1
              MOVE      HIGH-VALUE     TO   SIH-KEY
              GO   TO   900-SIH-READ-EXIT
     END-READ.
*
     IF       SIH-F405(1:4)       =   "9999"
              MOVE     1               TO   INV-FLG1
     ELSE
              ADD      1               TO   CNT-RDSIH
              MOVE     SIH-F405(1:4)   TO   WK-TENCDR
              MOVE     WK-TENCD        TO   SIH-KEY1
              MOVE     SIH-F402(1:9)   TO   WK-TORCDR
              MOVE     WK-TORCD        TO   SIH-KEY2
              MOVE     0               TO   INV-FLG1
     END-IF.
*
 900-SIH-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
