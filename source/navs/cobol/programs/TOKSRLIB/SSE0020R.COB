****************************************************************
*
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　
*    業務名　　　　　　　：　販売管理システム　　　　　　　　　
*    モジュール名　　　　：　請求合計ファイル作成　　　　　　　
*    作成日／更新日　　　：　92/12/03
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　
*    処理概要　　　　　　：　条件Ｆの締め日と一致した取引先の　
*                        ：　請求金額を集計して，請求合計Ｆに　
*                        ：　出力する。　　　　　　　　　　　　
*    作成日／更新日　　　：　2011/08/02
*    作成者／更新者　　　：　ＮＡＶ武井　　　　　　　　　　　　
*    修正概要　　　　　　：　請求合計Ｆに発注日，納品日設定。　
*    更新履歴            ：
*      2011/10/06 飯田/NAV 基幹サーバ統合
*      2012/03/04 三浦  基幹サーバ統合
*    更新日        　　　：　2012/09/03
*    更新者　　        　：　ＮＡＶ武井　　　　　　
*    修正概要　　　　　　：　山新データ（１０４１）は明細単位で
*                        ：　税計算を行う。　　　　　　　　　　
*    更新日        　　　：　2012/11/12
*    更新者　　        　：　ＮＡＶ高橋　　　　　　
*    修正概要　　　　　　：　税込単価を（前）項目より取得して、
*                        ：　請求金額を算出する様に変更する。　
*    更新日        　　　：　2014/01/06
*    更新者　　        　：　ＮＡＶ高橋　　　　　　
*    修正概要　　　　　　：　山新の税対象処理を変更する。
*    更新日        　　　：　2019/09/27
*    更新者　　        　：　ＮＡＶ高橋　　　　　　
*    修正概要　　　　　　：　消費税軽減税率対応
*    更新日        　　　：　2019/11/06
*    更新者　　        　：　ＮＡＶ高橋　　　　　　
*    修正概要　　　　　　：　指定取引先のみ実行版
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSE0020R.
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
                                  *> 2011/10/06,S  S.I/NAV
                                       DEN-F07   DEN-F112
                                  *> 2011/10/06,E  S.I/NAV
                                       DEN-F03
                        STATUS         DEN-ST.
*----<< 条件ファイル >>--*
     SELECT   HJYOKEN   ASSIGN         DA-01-VI-JYOKEN1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  JYO-F01   JYO-F02
                        STATUS         JYO-ST.
*----<< 請求合計Ｆ >>--*
     SELECT   HSEIGKF   ASSIGN         DA-01-VI-SETGKFR2
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  SEI-F01   SEI-F03
                                       SEI-F04   SEI-F05
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
 FD  HSEIGKF            LABEL RECORD   IS   STANDARD.
     COPY     SETGKFA   OF        XFDLIB
              JOINING   SEI       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  END-FLG        PIC  9(01).
     03  INV-FLG        PIC  9(01).

     03  SEI-INV-FLG    PIC  X(03)  VALUE  SPACE.
 01  COUNTERS.
     03  TOK-CNT        PIC  9(06).
     03  DEN-CNT        PIC  9(06).
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
***  日付変換用  2011/08/02
 01  WKDEN-F111        PIC  9(08).
 01  WKDEN-F112        PIC  9(08).
***  ADD  2012/09/03
 01  WK-ZEI-KIKANYMD.
     03  WK-ZEI-KIKANYMD9       PIC   9(09).
     03  WK-ZEI-KIKANFIL        PIC   99.
 01  WK-ZEI-KIKANYMD-R  REDEFINES  WK-ZEI-KIKANYMD
                      PIC  9(09)V99.
 01  WK-DENF181        PIC S9(11).
 01  WK-DENF172        PIC S9(11).
**
*----<< BREAK KEY >>--*
 01  BREAK-KEY.
     03  NEW.
         05  NEW-TOR                        PIC  9(08).
         05  NEW-DEN                        PIC  X(09).
         05  NEW-TEN                        PIC  9(05).
         05  NEW-NOHINBI                    PIC  9(08).
     03  OLD.
         05  OLD-TOR                        PIC  9(08).
         05  OLD-DEN                        PIC  X(09).
         05  OLD-TEN                        PIC  9(05).
         05  OLD-NOHINBI                    PIC  9(08).
*伝票番号編集用
 01  WK-DEN-F02.
     03  WK-DEN-F02-1   PIC  9(08)          VALUE  ZERO.
     03  WK-DEN-F02-2   PIC  9(01)          VALUE  ZERO.
*#2019/09/18 NAV ST  消費税率取得サブ用
 01  WK-ZEIHENKAN.
     03  LINK-ZEIKBN              PIC  X(01).
     03  LINK-ZEIDATE             PIC  9(08).
     03  LINK-ZEIERR              PIC  X(01).
     03  LINK-ZEIRITU             PIC  9(05).
     03  LINK-DZEIRITU            PIC  9(05).
     03  LINK-ZEIRITU-H           PIC  9(03)V9(02).
     03  LINK-DZEIRITU-H          PIC  9(03)V9(02).
*#2019/09/18 NAV ED  消費税率取得サブ用
*
****************************************************************
 LINKAGE           SECTION.
****************************************************************
 01  LINK-TOKCD                   PIC  9(08).
 01  LINK-KENSU                   PIC  9(07).
*
****************************************************************
 PROCEDURE              DIVISION         USING  LINK-TOKCD
                                                LINK-KENSU.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 取引先マスタ >>--*
 HTOKMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTOKMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SSE0020R HTOKMS ERROR " TOK-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
**** CLOSE    HTOKMS    HDENJNL   HJYOKEN   HSEIGKF.
     STOP     RUN.
*----<< 伝票データ >>--*
 HDENJNL-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HDENJNL.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SSE0020R HDENJNL ERROR " DEN-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
**** CLOSE    HTOKMS    HDENJNL   HJYOKEN   HSEIGKF.
     STOP     RUN.
*----<< 条件ファイル >>--*
 HJYOKEN-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HJYOKEN.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY  "### SSE0020R HJYOKEN ERROR " JYO-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
**** CLOSE    HTOKMS    HDENJNL   HJYOKEN   HSEIGKF.
     STOP     RUN.
*----<< 請求合計Ｆ >>--*
 HSEIGKF-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HSEIGKF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     MOVE     4000           TO   PROGRAM-STATUS.
     DISPLAY "DEN-F01 = " DEN-F01 " F02 = " DEN-F02 UPON CONS.
     DISPLAY "SEI-F01 = " SEI-F01 " F05 = " SEI-F05 UPON CONS.
     DISPLAY  "### OSKT320 HSEIGKF ERROR " SEI-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
**** CLOSE    HTOKMS    HDENJNL   HJYOKEN   HSEIGKF.
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
     DISPLAY  "*** SSE0020R START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     OPEN     INPUT     HTOKMS.
     OPEN     I-O       HDENJNL.
     OPEN     INPUT     HJYOKEN.
     OPEN     I-O       HSEIGKF.
*----<< ﾜｰｸ ｼｮｷｾｯﾄ >>-*
     INITIALIZE         FLAGS.
     INITIALIZE         COUNTERS.
*
     MOVE     59        TO   JYO-F01.
     MOVE     SPACE     TO   JYO-F02.
     PERFORM  900-JYO-READ.
     IF       INV-FLG   =    0
              MOVE      JYO-F04   TO   SIME-DATE SSKTLSTD-DATE
              IF   SIME-DD   =    31
                   CALL "OSKTLSTD"     USING     SSKTLSTD-DATE
                                                 SSKTLSTD-RET
              END-IF
     ELSE
              MOVE      1         TO   END-FLG
     END-IF.
***  ADD  2012/09/03
*  消費税率取得
     MOVE     99        TO   JYO-F01.
     MOVE    "ZEI"      TO   JYO-F02.
     PERFORM  900-JYO-READ.
     IF       INV-FLG   =    1
              MOVE      1         TO   END-FLG
     ELSE
              MOVE      JYO-F06   TO   WK-ZEI-KIKANYMD-R
     END-IF.
***
*
     IF       END-FLG   =    0
              PERFORM   900-TOK-READ
     END-IF.
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
     MOVE     LOW-VALUE      TO   BREAK-KEY.
     MOVE     SPACE          TO   SEI-REC.
     INITIALIZE                   SEI-REC.
*
     MOVE     TOK-F01        TO   DEN-F01.
     DISPLAY NC"＃処理中＝　" TOK-F01 NC"　＃" UPON CONS.
     INITIALIZE         DEN-F23.
     INITIALIZE         DEN-F04.
     INITIALIZE         DEN-F051.
     INITIALIZE         DEN-F03.
* 2011/10/06,S  S.I/NAV
     MOVE  ZERO             TO  DEN-F07.  *> 店舗CD
     MOVE  ZERO             TO  DEN-F112. *> 納品日
* 2011/10/06,E  S.I/NAV
     PERFORM  900-DEN-START-READ.
     MOVE     NEW            TO   OLD.
     PERFORM  210-SYUKEI
                   UNTIL     NEW       =    HIGH-VALUE.
     IF       SEI-F01        =    TOK-F01
     AND      SEI-F06   NOT  =    ZERO
*                  存在チェック
                   PERFORM   SEI-READ-SEC
                   IF   SEI-INV-FLG = "INV"
                        PERFORM   220-SEI-OUT
                   ELSE
                        DISPLAY "SEI-F01 1 = " SEI-F01 UPON CONS
                        DISPLAY "SEI-F03 1 = " SEI-F03 UPON CONS
                        DISPLAY "SEI-F04 1 = " SEI-F04 UPON CONS
                        DISPLAY "SEI-F05 1 = " SEI-F05 UPON CONS
                   END-IF
**************PERFORM   220-SEI-OUT
     END-IF.
*
     PERFORM  900-TOK-READ.
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
     CLOSE    HTOKMS.
     CLOSE    HDENJNL.
     CLOSE    HJYOKEN.
     CLOSE    HSEIGKF.
*
     DISPLAY  "+++ ﾄｸｲｻｷ ｹﾝｽｳ =" TOK-CNT " +++" UPON CONS.
     DISPLAY  "+++ ﾃﾞﾝﾋﾟｮｳﾃﾞｰﾀ=" DEN-CNT " +++" UPON CONS.
     DISPLAY  "+++ OUTPUT     =" OUT-CNT " +++" UPON CONS.
     MOVE     OUT-CNT        TO   LINK-KENSU.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SSE0020R END *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL  3     ｼｭｳｹｲ                                        *
*--------------------------------------------------------------*
 210-SYUKEI             SECTION.
     IF       NEW  NOT  =    OLD
              IF   SEI-F06   NOT  =    ZERO
*                  存在チェック
                   PERFORM   SEI-READ-SEC
                   IF   SEI-INV-FLG = "INV"
                        PERFORM   220-SEI-OUT
                   ELSE
                        DISPLAY "SEI-F01 2 = " SEI-F01 UPON CONS
                        DISPLAY "SEI-F03 2 = " SEI-F03 UPON CONS
                        DISPLAY "SEI-F04 2 = " SEI-F04 UPON CONS
                        DISPLAY "SEI-F05 2 = " SEI-F05 UPON CONS
                   END-IF
              END-IF
              MOVE      SPACE     TO   SEI-REC
              INITIALIZE               SEI-REC
     END-IF.
*
     IF       DEN-F04   =    1    OR   3    OR   5
                        OR   7    OR   9
******        MULTIPLY       -1   BY   DEN-F182
              MULTIPLY       -1   BY   DEN-F181
              MULTIPLY       -1   BY   DEN-F15
     END-IF.
     IF       DEN-F051  =    41   OR   42   OR   47   OR   48
******        MULTIPLY       -1   BY   DEN-F182
              MULTIPLY       -1   BY   DEN-F181
              MULTIPLY       -1   BY   DEN-F15
     END-IF.
     MOVE     DEN-F01        TO   SEI-F01.
     MOVE     SSKTLSTD-DATE  TO   SEI-F02.
     MOVE     DEN-F07        TO   SEI-F03.
     MOVE     DEN-F113       TO   SEI-F04.
*****MOVE     DEN-F02        TO   SEI-F05. 93/03/05. ﾍﾝｺｳ H.K
     MOVE     DEN-F23        TO   SEI-F05.
*2008/10/17ｻﾝﾃﾞｰ:30402で商区が"1"の場合、伝票番号末尾に0ｾｯﾄ
     IF       DEN-F01  =  30402
              IF  DEN-F131 = "1 "
                  MOVE  ZERO       TO  WK-DEN-F02-2
                  MOVE  DEN-F02    TO  WK-DEN-F02-1
                  MOVE  WK-DEN-F02 TO  SEI-F05
              END-IF
     END-IF.
***  ADD      DEN-F182       TO   SEI-F06.
*2012/19/03  山新（１０４１）は税計算
*%%%%% 2014/01/06 NAV ST ↓山新税抜対応終了の為コメントアウト
*%   IF       DEN-F01  =  1041
*##2012/11/12 ↓ 下記、算出方法を変更 *##部分をｺﾒﾝﾄｱｳﾄ
*##      IF  WK-ZEI-KIKANYMD9  >  DEN-F113
**************COMPUTE  WK-DENF181  =   DEN-F181 *  JYO-F04
*変更（原価単価×消費税率）×数量で原価金額算出に変更
*##           COMPUTE WK-DENF172 ROUNDED = DEN-F172 * JYO-F04
*##           COMPUTE WK-DENF181 = DEN-F15  * WK-DENF172
*##      ELSE
**************COMPUTE  WK-DENF181  =   DEN-F181 *  JYO-F05
*変更（原価単価×消費税率）×数量で原価金額算出に変更
*##           COMPUTE WK-DENF172 ROUNDED = DEN-F172 * JYO-F05
*##           COMPUTE WK-DENF181 = DEN-F15  * WK-DENF172
*##      END-IF
*%%    IF  DEN-F512 NOT =  ZERO
*%%        MOVE    DEN-F512     TO   WK-DENF172
*%%    ELSE
*********（前）エリア項目が０の場合、消費税率で計算する。
*%%      IF  WK-ZEI-KIKANYMD9  >  DEN-F113
*変更（原価単価×消費税率）×数量で原価金額算出に変更
*%%           COMPUTE WK-DENF172 ROUNDED = DEN-F172 * JYO-F04
*%%           COMPUTE WK-DENF181 = DEN-F15  * WK-DENF172
*%%      ELSE
*変更（原価単価×消費税率）×数量で原価金額算出に変更
*%%           COMPUTE WK-DENF172 ROUNDED = DEN-F172 * JYO-F05
*%%           COMPUTE WK-DENF181 = DEN-F15  * WK-DENF172
*%%      END-IF
*%%    END-IF
*%%    COMPUTE WK-DENF181 = DEN-F15  * WK-DENF172
*%%    ADD     WK-DENF181   TO   SEI-F06
*##2012/11/12 ↑ 下記、算出方法を変更 *##部分をｺﾒﾝﾄｱｳﾄ
*%%  ELSE
*%%    ADD      DEN-F181   TO   SEI-F06
*%%  END-IF.
*%% ↓下記のコメントアウトを復活
     ADD      DEN-F181       TO   SEI-F06.
*%%%%% 2014/01/06 NAV ST ↑山新税抜対応終了の為コメントアウト
     MOVE     DEN-F051       TO   SEI-F07.
     MOVE     DEN-F133       TO   SEI-F08.
     ADD      DEN-F15        TO   SEI-F09.
     MOVE     DEN-F112       TO   SEI-F10.
     MOVE     DEN-F08        TO   SEI-F11.
     MOVE     DEN-F12        TO   SEI-F12.
***  2011/08/02
     MOVE     DEN-F111       TO   WKDEN-F111.
     MOVE     DEN-F112       TO   WKDEN-F112.
     MOVE     WKDEN-F111     TO   SEI-F13.
     MOVE     WKDEN-F112     TO   SEI-F14.
*#2019/09/18 NAV ST 消費税軽減税率対応
*    税区分
*****MOVE     DEN-F34             TO   SEI-F15.
*****消費税率取得サブ
     INITIALIZE                        WK-ZEIHENKAN.
     IF       DEN-F34  =  SPACE  OR "0"
              MOVE   "0"          TO   LINK-ZEIKBN  SEI-F15
     ELSE
              MOVE   DEN-F34      TO   LINK-ZEIKBN  SEI-F15
     END-IF.
*****MOVE     DEN-F112            TO   LINK-ZEIDATE.
     MOVE     DEN-F113            TO   LINK-ZEIDATE.
*
     CALL  "SKYTAXPG"  USING LINK-ZEIKBN LINK-ZEIDATE LINK-ZEIERR
                             LINK-ZEIRITU LINK-DZEIRITU.
     IF     LINK-ZEIERR  NOT =  "0"
            MOVE      4000        TO   PROGRAM-STATUS
            DISPLAY NC"＃＃消費税率取得エラー＃＃" UPON CONS
            DISPLAY NC"＃＃税区分" " = " LINK-ZEIKBN UPON CONS
           DISPLAY NC"＃＃日付　" " = " LINK-ZEIDATE   UPON CONS
            STOP  RUN
     ELSE
            COMPUTE  LINK-ZEIRITU-H =  LINK-ZEIRITU / 100
            COMPUTE  LINK-DZEIRITU-H =  LINK-DZEIRITU / 100
     END-IF.
*
     IF     LINK-ZEIERR  =  "1"
            MOVE     LINK-DZEIRITU-H   TO  SEI-F16
     ELSE
            MOVE     LINK-ZEIRITU-H    TO  SEI-F16
     END-IF.
*#2019/09/18 NAV ED 消費税軽減税率対応
*
     MOVE     NEW            TO   OLD.
     PERFORM  900-DEN-READ.
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
     DISPLAY  "### SSE0020R HJYOKEN INVALID KEY=" JYO-F01 " ###"
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
*指定されて取引先のみ
     IF       TOK-F01  NOT =  LINK-TOKCD
              GO   TO   900-TOK-READ
     END-IF.
*
     IF       TOK-F12 (1)    NOT  =    SIME-DD
     AND      TOK-F12 (2)    NOT  =    SIME-DD
     AND      TOK-F12 (3)    NOT  =    SIME-DD
              GO   TO   900-TOK-READ
     END-IF.
*
     MOVE     ZERO           TO        DEN-CNT.
     DISPLAY NC"処理取引先" " = " TOK-F01  UPON  CONS.
*
     ADD      1         TO   TOK-CNT.
 900-TOK-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    伝票ファイル　 START READ                    *
*--------------------------------------------------------------*
 900-DEN-START-READ     SECTION.
* 2011/10/06,S  S.I/NAV
**     START    HDENJNL   KEY  >=   DEN-F01   DEN-F23
**                                  DEN-F04   DEN-F051
**                                  DEN-F03
**              INVALID   KEY
**                        MOVE HIGH-VALUE     TO   NEW
**     END-START.
     START    HDENJNL   KEY  >=   DEN-F01   DEN-F23
                                  DEN-F04   DEN-F051
                                  DEN-F07   DEN-F112
                                  DEN-F03
              INVALID   KEY
                        MOVE HIGH-VALUE     TO   NEW
     END-START.
* 2011/10/06,E  S.I/NAV
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
              GO   TO   900-DEN-READ-EXIT
     END-READ.
*取引先ＣＤが異なる時
     IF       DEN-F01   NOT  =    TOK-F01
              PERFORM   900-DEN-REWRITE
              MOVE      HIGH-VALUE     TO   NEW
              GO   TO   900-DEN-READ-EXIT
     END-IF.
*請求ＦＬＧ
     IF       DEN-F261  NOT  =    0    AND  1
              PERFORM   900-DEN-REWRITE
              GO   TO   900-DEN-READ
     END-IF.
*行番号＝９０
     IF       DEN-F03   =    90
              MOVE      1    TO   DEN-F261
              PERFORM   900-DEN-REWRITE
              GO   TO   900-DEN-READ
     END-IF.
*売上データ
     IF       DEN-F277  NOT  =    9
              PERFORM   900-DEN-REWRITE
              GO   TO   900-DEN-READ
     END-IF.
*画面指定月末日
     IF       DEN-F112  >    SSKTLSTD-DATE
              PERFORM   900-DEN-REWRITE
              GO   TO   900-DEN-READ
     END-IF.
*行番号＝８０
     IF       DEN-F03   =    80
              MOVE      1    TO   DEN-F261
              PERFORM   900-DEN-REWRITE
              GO   TO   900-DEN-READ
     END-IF.
*請求ＦＬＧセット
     MOVE     1         TO   DEN-F261.
     PERFORM  900-DEN-REWRITE.
*\\ 93/08/05  START \\
*
     IF       DEN-F133  =    9
              GO   TO   900-DEN-READ
     END-IF.
*\\ 93/08/05  END   \\
*#2019/09/27 NAV ST 取Ｍの請求区分＝空白（しない）の場合は
*#                  請求データは作成しない。
     IF       TOK-F90   =    "1"
              CONTINUE
     ELSE
              GO   TO   900-DEN-READ
     END-IF.
*#2019/09/27 NAV ED
     ADD      1         TO   DEN-CNT.
*
     IF  DEN-CNT(4:3) = "500" OR "000"
         DISPLAY "## DEN-CNT = " DEN-CNT  UPON CONS
     END-IF.
*
     MOVE     DEN-F01        TO   NEW-TOR.
     MOVE     DEN-F23        TO   NEW-DEN.
     MOVE     DEN-F07        TO   NEW-TEN.
     MOVE     DEN-F112       TO   NEW-NOHINBI.
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
*    LEVEL ALL    請求合計ファイル読み込み
*--------------------------------------------------------------*
 SEI-READ-SEC           SECTION.
*
     READ     HSEIGKF
              INVALID      MOVE  "INV"  TO  SEI-INV-FLG
              NOT  INVALID MOVE  SPACE  TO  SEI-INV-FLG
     END-READ.
*
 SEI-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
