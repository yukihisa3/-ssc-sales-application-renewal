****************************************************************
*
*    顧客名　　　　　　　：　サカタのタネ（株）殿　　　　　　　
*    業務名　　　　　　　：　出荷
*    サブシステム　　　　：　出荷管理システム
*    モジュール名　　　　：　新発注集計データ店舗順取得
*    作成日／更新日　　　：　2022/05/12
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　
*    処理概要　　　　　　：　新発注集計データに
*                        ：　ＥＸＣＥＬ展開順をセットする。
*    更新履歴            ：
*
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            CSV2020B.
*                  流用:CSV2010B
 AUTHOR.                NAV.
 DATE-WRITTEN.          2022/05/12.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SPECIAL-NAMES.
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*新発注集計データ
     SELECT   NHTMEIF   ASSIGN         NHTMEIL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE IS   DYNAMIC
                        RECORD    KEY  IS   HME-F01
                                            HME-F02
                                            HME-F03
                                            HME-F04
                                            HME-F05
                                            HME-F06
                                            HME-F15
                        STATUS         IS   HME-ST.
*新発注集計店舗データ
     SELECT   NHTTENF   ASSIGN         NHTTENL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE IS   RANDOM
                        RECORD    KEY  IS   HTE-F01
                        STATUS         IS   HTE-ST.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
****************************************************************
*    FILE = 新発注集計データ　　　　                           *
****************************************************************
 FD  NHTMEIF            LABEL     RECORD   IS   STANDARD.
                        COPY      NHTMEIF  OF   XFDLIB
                        JOINING   HME      AS   PREFIX.
****************************************************************
*    FILE = 新発注集計店舗データ　　                           *
****************************************************************
 FD  NHTTENF            LABEL     RECORD   IS   STANDARD.
                        COPY      NHTTENF  OF   XFDLIB
                        JOINING   HTE      AS   PREFIX.
****************************************************************
 WORKING-STORAGE        SECTION.
****************************************************************
*ステータス領域
 01  DEN-ST             PIC  X(02).
 01  SHO-ST             PIC  X(02).
 01  SUB-ST             PIC  X(02).
 01  TEN-ST             PIC  X(02).
 01  HME-ST             PIC  X(02).
 01  HTE-ST             PIC  X(02).
*プログラムＩＤ
 01  PG-ID.
     03  ID-PG          PIC  X(08)     VALUE  "CSV2020B".
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
     03  END-FLG         PIC  X(03).
     03  HSHOTBL-INV-FLG PIC  X(03)  VALUE  SPACE.
     03  SUBMEIF-INV-FLG PIC  X(03)  VALUE  SPACE.
     03  HTENMS-INV-FLG  PIC  X(03)  VALUE  SPACE.
     03  NHTMEIF-INV-FLG PIC  X(03)  VALUE  SPACE.
     03  NHTTENF-INV-FLG PIC  X(03)  VALUE  SPACE.
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
     03  CNT-READ            PIC  9(07).
     03  CNT-REWRITE-HME     PIC  9(07).
     03  CNT-WRITE-HTE       PIC  9(05).
     03  CNT-SEQ             PIC  9(05).
*
******************************************************************
 LINKAGE           SECTION.
******************************************************************
*
*
*
****************************************************************
 PROCEDURE              DIVISION.
****************************************************************
****************************************************************
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
****************************************************************
 DECLARATIVES.
*新発注集計データ
 HME-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      NHTMEIF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  SEC-NAME       UPON CONS.
     DISPLAY  "### " PG-ID " NHTMEIF ERROR " HME-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     MOVE     "4000"         TO   PROGRAM-STATUS.
     STOP     RUN.
*新発注集計店舗データ
 HTE-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      NHTTENF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  SEC-NAME       UPON CONS.
     DISPLAY  "### " PG-ID " NHTTENF ERROR " HTE-ST " "
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
*
*終了メッセージ出力
     DISPLAY  NC"　発注データ対象＝　" CNT-REWRITE-HME
                                                     UPON CONS.
     DISPLAY  NC"　　　　　　更新＝　" CNT-REWRITE-HME
                                                     UPON CONS.
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
     OPEN     INPUT     NHTTENF.
     OPEN     I-O       NHTMEIF
*
*初期化
     INITIALIZE    FLG-AREA  WRK-AREA  CNT-AREA.
*
*新発注集計データＲＥＡＤ
     PERFORM  900-HME-READ.
     IF       END-FLG  =  "END"
              DISPLAY  NC"　＃対象データなし＃"  UPON CONS
              MOVE     4010       TO   PROGRAM-STATUS
              GO                  TO   100-INIT-RTN-EXIT
     END-IF.
*
 100-INIT-RTN-EXIT.
     EXIT.
****************************************************************
*    LEVEL  2      ﾒｲﾝ ｼｮﾘ                                     *
****************************************************************
 200-MAIN-RTN           SECTION.
     MOVE     "200-MAIN-RTN"      TO   S-NAME.
*
*    新発注集計データ更新
     PERFORM  210-REWRITE-RTN.
*
*    新発注集計データＲＥＡＤ
     PERFORM  900-HME-READ.
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
     CLOSE    NHTMEIF
              NHTTENF.
*
 300-END-RTN-EXIT.
     EXIT.
****************************************************************
*    LEVEL  3     新発注集計データ更新　　　　　　             *
****************************************************************
 210-REWRITE-RTN          SECTION.
     MOVE     "210-REWRITE-RTN"     TO   S-NAME.
*
 210-REWRITE-RTN01.
*    存在チェック
     MOVE     HME-F15        TO   HTE-F01.
     PERFORM  930-HTE-READ.
*    更新
     IF  NHTTENF-INV-FLG  =  "INV"
         CONTINUE
     ELSE
         MOVE     HTE-F03        TO   HME-F16
         REWRITE  HME-REC
         ADD      1              TO   CNT-REWRITE-HME
     END-IF.
*
 210-REWRITE-RTN-EXIT.
     EXIT.
****************************************************************
*    LEVEL ALL    新発注集計データ　 READ                      *
****************************************************************
 900-HME-READ           SECTION.
     MOVE     "900-HME-READ"      TO   S-NAME.
*
     READ     NHTMEIF
         NEXT AT END
              MOVE     "END"      TO   END-FLG
              GO        TO        900-HME-READ-EXIT
     END-READ.
*
     ADD      1         TO        CNT-READ.
*
 900-HME-READ-EXIT.
     EXIT.
****************************************************************
*    LEVEL ALL    新発注集計店舗データ検索　　　               *
****************************************************************
 930-HTE-READ           SECTION.
     MOVE     "930-HTE-READ"      TO   S-NAME.
*
     READ     NHTTENF
         INVALID
         MOVE      "INV"          TO   NHTTENF-INV-FLG
         NOT  INVALID
         MOVE      SPACE          TO   NHTTENF-INV-FLG
     END-READ.
*
 930-HTE-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
