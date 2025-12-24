# CSV0100R

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/CSV0100R.COB`

## ソースコード

```cobol
****************************************************************
*
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　
*    サブシステム　　　　：　出荷管理システム
*    業務名　　　　　　　：　オンライン発注集計表　　　　　　　
*    モジュール名　　　　：　オンライン発注集計データ抽出　　　
*    作成日／更新日　　　：　99/09/14
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　
*    処理概要　　　　　　：　売上伝票ファイルを読み、発注集計表
*                        ：　プリントファイルを作成する。　　　
*    更新履歴            ：
*      2011/10/06 飯田/NAV 基幹サーバ統合
*      2020/07/17 ＮＡＶ高橋　在庫引当、小売連携、ＳＴＮＯ管理
*      2021/04/15 ＮＡＶ高橋　在庫引当判定変更（数量＝０の時）
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            CSV0100R.
 AUTHOR.                HAGIWARA.
 DATE-WRITTEN.          99/09/14.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SPECIAL-NAMES.
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*売上伝票ファイル
     SELECT  SHTDENF   ASSIGN    TO        SHTDENLA
                       ORGANIZATION        INDEXED
                       ACCESS    MODE      SEQUENTIAL
                  *> 2011/10/06,S  S.I/NAV
                  *>     RECORD    KEY       DEN-F46
                  *>                         DEN-F47
                  *>                         DEN-F01
                  *>                         DEN-F48
                  *>                         DEN-F02
                  *>                         DEN-F04
                  *>                         DEN-F132
                  *>                         DEN-F03
                       RECORD    KEY       DEN-F46
                                           DEN-F47
                                           DEN-F01
                                           DEN-F48
                                           DEN-F02
                                           DEN-F04
                                           DEN-F051
                                           DEN-F07
                                           DEN-F112
                                           DEN-F03
                  *> 2011/10/06,E  S.I/NAV
                       FILE      STATUS    DEN-ST.
*商品コード変換ファイル
     SELECT   HSHOTBL   ASSIGN         SHOTBL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  SHO-F01  SHO-F02
                        STATUS         SHO-ST.
*# 2020/07/17 NAV ST
*商品名称マスタ
     SELECT   HMEIMS    ASSIGN         MEIMS1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  MEI-F011  MEI-F0121
                                       MEI-F0122 MEI-F0123
                        STATUS         MEI-ST.
*# 2020/07/17 NAV ED
*発注集計プリントファイル
     SELECT   HACYUPPT  ASSIGN         HACYUPPT
                        ORGANIZATION   SEQUENTIAL
                        STATUS         HAC-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
****************************************************************
*    FILE = 売上伝票ファイル                                   *
****************************************************************
 FD  SHTDENF            LABEL     RECORD   IS   STANDARD.
                        COPY      SHTDENF  OF   XFDLIB
                        JOINING   DEN      AS   PREFIX.
****************************************************************
*    FILE = 商品コード変換テーブル                             *
****************************************************************
 FD  HSHOTBL            LABEL     RECORD   IS   STANDARD.
                        COPY      HSHOTBL  OF   XFDLIB
                        JOINING   SHO      AS   PREFIX.
*# 2020/07/17 NAV ST
****************************************************************
*    FILE = 商品名称マスタ　　　　　                           *
****************************************************************
 FD  HMEIMS             LABEL     RECORD   IS   STANDARD.
                        COPY      HMEIMS   OF   XFDLIB
                        JOINING   MEI      AS   PREFIX.
*# 2020/07/17 NAV ED
****************************************************************
*    FILE = 発注集計プリントファイル                           *
****************************************************************
 FD  HACYUPPT           LABEL     RECORD   IS   STANDARD
                        BLOCK     CONTAINS 27   RECORDS.
                        COPY      HACYUPPT OF   XFDLIB
                        JOINING   HAC      AS   PREFIX.
****************************************************************
 WORKING-STORAGE        SECTION.
****************************************************************
*ステータス領域
 01  DEN-ST             PIC  X(02).
 01  SHO-ST             PIC  X(02).
 01  HAC-ST             PIC  X(02).
*# 2020/07/17 NAV ST
 01  MEI-ST             PIC  X(02).
*# 2020/07/17 NAV ST
*プログラムＩＤ
 01  PG-ID.
     03  ID-PG          PIC  X(08)     VALUE  "VDA1700B".
*日付ワーク
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
*フラグ領域
 01  FLG-AREA.
     03  END-FLG        PIC  X(03).
*# 2020/07/17 NAV ST
     03  HMEIMS-INV-FLG PIC  X(03)  VALUE  SPACE.
*# 2020/07/17 NAV ED
*ワーク領域
 01  WRK-AREA.
     03  WK-NONYU-DATE  PIC  9(08).
*エラーセクション名
 01  SEC-NAME.
     03  FILLER         PIC  X(05)  VALUE " *** ".
     03  S-NAME         PIC  X(30).
*
*カウント領域
 01  CNT-AREA.
     03  CNT-READ            PIC  9(08).
     03  CNT-WRITE           PIC  9(08).
*
******************************************************************
 LINKAGE           SECTION.
******************************************************************
 01  BATNO.
     03  LINK-DATE           PIC  9(08).
     03  LINK-TIME           PIC  9(04).
     03  LINK-TORICD         PIC  X(08).
 01  SOKOCD.
     03  LINK-SOKO-KAISI     PIC  X(02).
     03  LINK-SOKO-OWARI     PIC  X(02).
 01  LINK-TENST              PIC  9(05).
 01  LINK-TENED              PIC  9(05).
*
****************************************************************
 PROCEDURE              DIVISION  USING  BATNO  SOKOCD
                                  LINK-TENST LINK-TENED.
****************************************************************
****************************************************************
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
****************************************************************
 DECLARATIVES.
*売上伝票データ
 DEN-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SHTDENF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  SEC-NAME       UPON CONS.
     DISPLAY  "### " PG-ID " SHTDENF ERROR " DEN-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     MOVE     "4000"         TO   PROGRAM-STATUS.
     STOP     RUN.
*商品コード変換テーブル
 SHO-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HSHOTBL.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  SEC-NAME       UPON CONS.
     DISPLAY  "### " PG-ID " HSHOTBL ERROR " SHO-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     MOVE     "4000"         TO   PROGRAM-STATUS.
     STOP     RUN.
*# 2020/07/17 NAV ST
*商品コード変換テーブル
 MEI-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HMEIMS.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  SEC-NAME       UPON CONS.
     DISPLAY  "### " PG-ID " HMEIMS ERROR " MEI-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     MOVE     "4000"         TO   PROGRAM-STATUS.
     STOP     RUN.
*# 2020/07/17 NAV ST
*発注集計プリントファイル
 HAC-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HACYUPPT.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  SEC-NAME       UPON CONS.
     DISPLAY  "### " PG-ID " HACYUPPT ERROR " HAC-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     MOVE     "4000"         TO   PROGRAM-STATUS.
     STOP     RUN.
 END DECLARATIVES.
****************************************************************
*    LEVEL   1     ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ                              *
****************************************************************
 000-PROG-CNTL          SECTION.
     MOVE     "000-PROG-CNTL"     TO   S-NAME.
*開始メッセージ出力
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** " PG-ID " START *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
******
     PERFORM  100-INIT-RTN.
     PERFORM  200-MAIN-RTN   UNTIL     END-FLG  =  "END".
     PERFORM  300-END-RTN.
******
*終了メッセージ出力
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** " PG-ID " END   *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
     STOP RUN.
*
 000-PROG-CNTL-EXIT.
     EXIT.
****************************************************************
*    LEVEL  2      ｼｮｷ ｼｮﾘ                                     *
****************************************************************
 100-INIT-RTN           SECTION.
     MOVE     "100-INIT-RTN"      TO   S-NAME.
*ファイルのオープン
     OPEN     INPUT     SHTDENF
                        HSHOTBL
*# 2020/07/17 NAV ST
                        HMEIMS
*# 2020/07/17 NAV ST
     OPEN     OUTPUT    HACYUPPT.
*初期化
     INITIALIZE    FLG-AREA  WRK-AREA  CNT-AREA.
     DISPLAY "BATNO  = " BATNO  UPON CONS.
     DISPLAY "SOKOCD = " SOKOCD UPON CONS.
     DISPLAY "TENST  = " LINK-TENST UPON CONS.
     DISPLAY "TENED  = " LINK-TENED UPON CONS.
*
*売上伝票データＲＥＡＤ
* 2011/10/06,S  S.I/NAV
     MOVE  SPACE            TO  DEN-REC.
     INITIALIZE  DEN-REC.
* 2011/10/06,E  S.I/NAV
     MOVE     LINK-DATE           TO   DEN-F46
     MOVE     LINK-TIME           TO   DEN-F47
     MOVE     LINK-TORICD         TO   DEN-F01
     MOVE     LINK-SOKO-KAISI     TO   DEN-F48
**     START    SHTDENF   KEY       >=   DEN-F46
**                                       DEN-F47
**                                       DEN-F01
**                                       DEN-F48
**     END-START.
     START    SHTDENF   KEY       >=   DEN-F46
                                       DEN-F47
                                       DEN-F01
                                       DEN-F48
                                       DEN-F02
                                       DEN-F04
                                       DEN-F051
                                       DEN-F07
                                       DEN-F112
                                       DEN-F03
     END-START.

     PERFORM  900-DEN-READ.
*
 100-INIT-RTN-EXIT.
     EXIT.
****************************************************************
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
****************************************************************
 200-MAIN-RTN           SECTION.
     MOVE     "200-MAIN-RTN"      TO   S-NAME.
*
*    発注集計表プリントファイル出力
     PERFORM  210-WRITE-RTN.
*    売上伝票データＲＥＡＤ
     PERFORM  900-DEN-READ.
*
 200-MAIN-RTN-EXIT.
     EXIT.
****************************************************************
*    LEVEL  2      ｴﾝﾄﾞ ｼｮﾘ                                    *
****************************************************************
 300-END-RTN            SECTION.
     MOVE     "300-END-RTN"       TO   S-NAME.
*
*ファイルのクローズ
     CLOSE    SHTDENF
              HSHOTBL
*# 2020/07/17 NAV ST
              HMEIMS
*# 2020/07/17 NAV ED
              HACYUPPT.
*
 300-END-RTN-EXIT.
     EXIT.
****************************************************************
*    LEVEL  3     発注集計表プリントファイル出力　             *
****************************************************************
 210-WRITE-RTN          SECTION.
     MOVE     "210-WRITE-RTN"     TO   S-NAME.
*
*レコード初期化
     MOVE     SPACE          TO   HAC-REC.
     INITIALIZE                   HAC-REC.
*    取引先コード
     MOVE     DEN-F01        TO   HAC-F01.
*    出荷倉庫コード
     MOVE     DEN-F48        TO   HAC-F02.
*    納入日
     MOVE     DEN-F112       TO   HAC-F03
*    量販店商品コード
     MOVE     DEN-F25        TO   HAC-F04.
*    サカタコード
***  商品変換テーブルＲＥＡＤ（売上伝票Ｆに自社商品コード無時）
     IF       DEN-F1411      =    SPACE
              MOVE     LINK-TORICD    TO   SHO-F01
              MOVE     DEN-F25        TO   SHO-F02
              PERFORM  910-SHO-READ
              MOVE     SHO-F031       TO   HAC-F05
              MOVE     SHO-F032       TO   HAC-F06
*** （売上伝票Ｆに自社商品コード有時）
     ELSE
              MOVE     DEN-F1411      TO   HAC-F05
              MOVE     DEN-F1412      TO   HAC-F06
     END-IF.
*    店舗コード
     MOVE     DEN-F07        TO   HAC-F07.
*    商品名
     MOVE     DEN-F142       TO   HAC-F11.
*    原単価
     MOVE     DEN-F172       TO   HAC-F12.
*    売単価
     MOVE     DEN-F173       TO   HAC-F13.
*    原価金額
     MOVE     DEN-F181       TO   HAC-F14.
*    売価金額
     MOVE     DEN-F182       TO   HAC-F15.
*    数量
     MOVE     DEN-F15        TO   HAC-F16.
*# 2020/07/17 NAV ST
*    在庫引当
     MOVE     DEN-F27D       TO   HAC-F17.
*****2021/04/15 NAV ST
     IF       DEN-F15  =  ZERO
              MOVE  1        TO   HAC-F17
     END-IF.
*****2021/04/15 NAV ED
*    小売連携
     MOVE     DEN-F32        TO   HAC-F18.
*    ＳＴＮＯ管理
     MOVE     DEN-F1411      TO   MEI-F011.
     MOVE     DEN-F1412      TO   MEI-F012.
     PERFORM  920-MEI-READ.
     IF  HMEIMS-INV-FLG  =  "INV"
         MOVE SPACE          TO   HAC-F19
     ELSE
         IF   MEI-F97 = 1
              MOVE   "1"     TO   HAC-F19
         ELSE
              MOVE   SPACE   TO   HAC-F19
         END-IF
     END-IF.
*# 2020/07/17 NAV ED
*    出力
     WRITE    HAC-REC.
     ADD      1              TO   CNT-WRITE.
*
 210-WRITE-RTN-EXIT.
     EXIT.
****************************************************************
*    LEVEL ALL    売上伝票データ　　 READ                      *
****************************************************************
 900-DEN-READ           SECTION.
     MOVE     "900-DEN-READ"      TO   S-NAME.
*
     READ     SHTDENF
         AT END
              MOVE     "END"      TO   END-FLG
              GO        TO        900-DEN-READ-EXIT
     END-READ.
*ブレイクチェック
     IF       DEN-F48   >      LINK-SOKO-OWARI  OR
              DEN-F01   NOT =  LINK-TORICD      OR
              DEN-F47   NOT =  LINK-TIME        OR
              DEN-F46   NOT =  LINK-DATE
              MOVE     "END"      TO   END-FLG
              GO        TO        900-DEN-READ-EXIT
     END-IF.
*
     IF       DEN-F07 >= LINK-TENST
     AND      DEN-F07 <= LINK-TENED
              CONTINUE
     ELSE
              GO        TO        900-DEN-READ
     END-IF.
*
     ADD      1         TO        CNT-READ.
*
 900-DEN-READ-EXIT.
     EXIT.
****************************************************************
*    LEVEL ALL    商品コード変換テーブル　READ                 *
****************************************************************
 910-SHO-READ           SECTION.
     MOVE     "910-SHO-READ"      TO   S-NAME.
*
     READ     HSHOTBL
         INVALID
         DISPLAY "商品テーブルエラー　取引先　＝　" SHO-F01
               "　店舗　＝　"  DEN-F07
               "　商品　＝　"  SHO-F02
              MOVE      SPACE     TO   SHO-F031
              MOVE      ZERO      TO   SHO-F04
     END-READ.
*
 910-SHO-READ-EXIT.
     EXIT.
*# 2020/07/17 NAV ST
****************************************************************
*    LEVEL ALL    商品名称マスタ読込　　　　　                 *
****************************************************************
 920-MEI-READ           SECTION.
     MOVE     "920-MEI-READ"      TO   S-NAME.
*
     READ     HMEIMS
         INVALID
         MOVE      "INV"          TO   HMEIMS-INV-FLG
         NOT  INVALID
         MOVE      SPACE          TO   HMEIMS-INV-FLG
     END-READ.
*
 920-MEI-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*

```
