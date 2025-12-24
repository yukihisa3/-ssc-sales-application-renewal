****************************************************************
*                                                              *
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　*
*    サブシステム　　　　：　出荷管理システム                  *
*    業務名　　　　　　　：　手書発注集計表　　　　　　　　　　*
*    モジュール名　　　　：　手書発注集計データ抽出　　　　　　*
*    作成日／更新日　　　：　08/03/18                          *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　売上伝票ファイルを読み、発注集計表*
*                        ：　プリントファイルを作成する。　　　*
*                                                              *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            CSV0125B.
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
     SELECT  DENPYOF   ASSIGN    TO        DENPYOF
                       FILE      STATUS    DEN-ST.
*商品コード変換ファイル
     SELECT   HSHOTBL   ASSIGN         SHOTBL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  SHO-F01  SHO-F02
                        STATUS         SHO-ST.
*発注集計プリントファイル
     SELECT   HACYUDT  ASSIGN         HACYUDT
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
 FD  DENPYOF            LABEL     RECORD   IS   STANDARD.
                        COPY      SHTDENF  OF   XFDLIB
                        JOINING   DEN      AS   PREFIX.
****************************************************************
*    FILE = 商品コード変換テーブル                             *
****************************************************************
 FD  HSHOTBL            LABEL     RECORD   IS   STANDARD.
                        COPY      HSHOTBL  OF   XFDLIB
                        JOINING   SHO      AS   PREFIX.
****************************************************************
*    FILE = 発注集計プリントファイル                           *
****************************************************************
 FD  HACYUDT           LABEL     RECORD   IS   STANDARD
                        BLOCK     CONTAINS 27   RECORDS.
                        COPY      HACYUDT OF   XFDLIB
                        JOINING   HAC      AS   PREFIX.
****************************************************************
 WORKING-STORAGE        SECTION.
****************************************************************
*ステータス領域
 01  DEN-ST             PIC  X(02).
 01  SHO-ST             PIC  X(02).
 01  HAC-ST             PIC  X(02).
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
*
****************************************************************
 PROCEDURE              DIVISION.
****************************************************************
****************************************************************
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
****************************************************************
 DECLARATIVES.
*売上伝票データ
 DEN-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      DENPYOF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  SEC-NAME       UPON CONS.
     DISPLAY  "### " PG-ID " DENPYOF ERROR " DEN-ST " "
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
*発注集計プリントファイル
 HAC-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HACYUDT.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  SEC-NAME       UPON CONS.
     DISPLAY  "### " PG-ID " HACYUDT ERROR " HAC-ST " "
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
     OPEN     INPUT     DENPYOF
                        HSHOTBL
     OPEN     OUTPUT    HACYUDT.
*初期化
     INITIALIZE    FLG-AREA  WRK-AREA  CNT-AREA.
*
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
     CLOSE    DENPYOF
              HSHOTBL
              HACYUDT.
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
     MOVE     DEN-F08        TO   HAC-F02.
*    納入日
     MOVE     DEN-F112       TO   HAC-F03
*    量販店商品コード
     MOVE     DEN-F25        TO   HAC-F04.
*    商品変換テーブル参照_番取得
     MOVE     DEN-F01        TO   SHO-F01.
     MOVE     DEN-F25        TO   SHO-F02.
     PERFORM  910-SHO-READ.
*    サカタコード
***  商品変換テーブルＲＥＡＤ（売上伝票Ｆに自社商品コード無時）
     IF       DEN-F1411      =    SPACE
              MOVE     SHO-F031       TO   HAC-F05
*** （売上伝票Ｆに自社商品コード有時）
     ELSE
              MOVE     DEN-F1411      TO   HAC-F05
     END-IF.
*    店舗コード
     MOVE     DEN-F07        TO   HAC-F06.
*    商品名
     MOVE     DEN-F142       TO   HAC-F10.
*    原単価
     MOVE     DEN-F172       TO   HAC-F11.
*    売単価
     MOVE     DEN-F173       TO   HAC-F12.
*    原価金額
     MOVE     DEN-F181       TO   HAC-F13.
*    売価金額
     MOVE     DEN-F182       TO   HAC-F14.
*    数量
     MOVE     DEN-F15        TO   HAC-F15.
*    バッチ日付
     MOVE     99999999       TO   HAC-F16.
*    バッチ時間
     MOVE     9999           TO   HAC-F17.
*    バッチ取引先
     MOVE     DEN-F01        TO   HAC-F18.
*    _番
     MOVE     SHO-F08        TO   HAC-F19.
*    伝票区分
     MOVE     DEN-F12        TO   HAC-YOBI.
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
     READ     DENPYOF
         AT END
              MOVE     "END"      TO   END-FLG
              GO        TO        900-DEN-READ-EXIT
     END-READ.
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
              MOVE      SPACE     TO   SHO-F08
     END-READ.
*
 910-SHO-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
