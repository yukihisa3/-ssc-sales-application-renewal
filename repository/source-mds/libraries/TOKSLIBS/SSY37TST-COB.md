# SSY37TST

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSY37TST.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　ナフコ出荷依頼リスト出力　　　　　*
*    業務名　　　　　　　：　出荷処理                         *
*    モジュール名　　　　：　出荷依頼リスト出力（商品別）      *
*    作成日／更新日　　　：　10/10/12                          *
*    作成者／更新者　　　：　NAV                               *
*    処理概要　　　　　　：　出荷依頼リストを出力する。　　　  *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
*
 PROGRAM-ID.            SSY37TST.
 AUTHOR.                NAV.
 DATE-WRITTEN.          10/10/12.
*
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
*
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM.
 OBJECT-COMPUTER.       FACOM.
 SPECIAL-NAMES.         CONSOLE   IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
****<<基本情報Ｆ　管理番号用 >>*********************************
     SELECT   NFJOHOL1           ASSIGN    TO   DA-01-VI-NFJOHOL1
                                 ORGANIZATION   INDEXED
                                 ACCESS  MODE   SEQUENTIAL
                                 RECORD  KEY    JH1-F01  JH1-F05
                                                JH1-F06  JH1-F07
                                                JH1-F08  JH1-F09
                                 STATUS         JH1-STATUS.
*
****<<テクノス連携ファイル　 >>*********************************
     SELECT   SNDTECNS           ASSIGN    TO   DA-01-S-SNDTECNS
                                 STATUS         SND-STATUS.
****************************************************************
 DATA                   DIVISION.
****************************************************************
*
 FILE                   SECTION.
*
*--------------------------------------------------------------*
*    FILE = ナフコ　　基本情報ファイル　管理_用
*--------------------------------------------------------------*
 FD  NFJOHOL1           LABEL RECORD   IS   STANDARD.
     COPY     NFJOHOF   OF        XFDLIB
              JOINING   JH1       PREFIX.
*
*--------------------------------------------------------------*
*    FILE = テクノス連携ファイル
*--------------------------------------------------------------*
 FD  SNDTECNS          LABEL     RECORD    IS   STANDARD.
     COPY     SNDTECNS   OF        XFDLIB
              JOINING    SND      PREFIX.
*----------------------------------------------------------------*
*             WORKING-STORAGE     SECTION                        *
*----------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
**** エンドフラグ
 01  END-FLG                      PIC       X(03)  VALUE  SPACE.
*
**** ステイタス　エリア
 01  JH1-STATUS                   PIC       X(02).
 01  SND-STATUS                   PIC       X(02).
*
***** システム日付ワーク
 01  SYSTEM-HIZUKE.
     03  SYSYMD                   PIC       9(06)  VALUE  ZERO.
     03  SYS-DATEW                PIC       9(08)  VALUE  ZERO.
     03  SYS-DATE-R               REDEFINES SYS-DATEW.
         05  SYS-YY               PIC       9(04).
         05  SYS-MM               PIC       9(02).
         05  SYS-DD               PIC       9(02).
*
***** カウンタ
 01  RD-CNT                       PIC       9(07)  VALUE  ZERO.
 01  WT-CNT                       PIC       9(07)  VALUE  ZERO.
*
***** メッセージエリア
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY37TST".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SSY37TST".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND1.
         05  FILLER               PIC       X(04)  VALUE
                       "### ".
         05  ERR-PG-ID            PIC       X(08)  VALUE
                       "SSY37TST".
         05  FILLER               PIC       X(10)  VALUE
                       " ABEND ###".
*
     03  MSG-ABEND2.
         05  FILLER               PIC       X(04)  VALUE
                       "### ".
         05  ERR-FL-ID            PIC       X(08).
         05  FILLER               PIC       X(04)  VALUE
                       " ST-".
         05  ERR-STCD             PIC       X(02).
         05  FILLER               PIC       X(04)  VALUE
                       " ###".
*
     03  SEC-NAME.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(07)  VALUE " SEC = ".
         05  S-NAME         PIC   X(30).
     03  MSG-IN.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " INPUT = ".
         05  IN-CNT         PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  MSG-OUT.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " OUTPG= ".
         05  OUT-CNT        PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
*日付サブルーチン用
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*パラメタ
 LINKAGE                SECTION.
 01  PARA-KANRINO           PIC   9(08).
*
****************************************************************
*                                                              *
*             ＭＡＩＮ　　　　　　ＭＯＤＵＬＥ                 *
*                                                              *
****************************************************************
*
****************************************************************
 PROCEDURE              DIVISION  USING    PARA-KANRINO.
****************************************************************
*
 DECLARATIVES.
 FILEERROR-SEC1         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE NFJOHOL1.
     MOVE     "NFJOHOL1"          TO        ERR-FL-ID.
     MOVE     JH1-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     STOP     RUN.
*
 FILEERROR-SEC2         SECTION.
     USE AFTER          EXCEPTION
                        PROCEDURE SNDTECNS.
     MOVE     "SNDTECNS"          TO        ERR-FL-ID.
     MOVE     SND-STATUS          TO        ERR-STCD.
     DISPLAY  MSG-ABEND1          UPON      CONS.
     DISPLAY  MSG-ABEND2          UPON      CONS.
     DISPLAY  SEC-NAME            UPON      CONS.
     STOP     RUN.
*
 END          DECLARATIVES.
****************************************************************
*             プロセス                      0.0                *
****************************************************************
 SSY37TST-START         SECTION.
*
     MOVE   "SSY37TST-START"      TO   S-NAME.
     PERFORM            INIT-SEC.
     PERFORM            MAIN-SEC  UNTIL     END-FLG   =  "END".
     PERFORM            END-SEC.
     STOP               RUN.
*
 SSY37TST-END.
     EXIT.
*
****************************************************************
*             初期処理                      1.0                *
****************************************************************
 INIT-SEC               SECTION.
*
     MOVE     "INIT-SEC"          TO   S-NAME.
     OPEN     INPUT     NFJOHOL1.
     OPEN     OUTPUT    SNDTECNS.
*
     DISPLAY  MSG-START UPON CONS.
*
     ACCEPT   SYSYMD    FROM      DATE.
     MOVE    "3"        TO        LINK-IN-KBN.
     MOVE     SYSYMD    TO        LINK-IN-YMD6.
     CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD8.
     IF       LINK-OUT-RET   =    ZERO
              MOVE      LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
              MOVE    ZERO             TO   SYS-DATEW
     END-IF.
*
     MOVE  SPACE                TO   JH1-REC.
     INITIALIZE                      JH1-REC.
     MOVE  PARA-KANRINO         TO   JH1-F01.
*
     START  NFJOHOL1  KEY  >=   JH1-F01  JH1-F05  JH1-F06
                                JH1-F07  JH1-F08  JH1-F09
            INVALID   KEY
            MOVE     "END"      TO   END-FLG
            DISPLAY NC"＃対象データＳＴ無し＃" UPON CONS
            GO                  TO   INIT-EXIT
     END-START.
*
     PERFORM  NFJOHOL1-RD-SEC.
     IF  END-FLG   =   "END"
         DISPLAY NC"＃対象データＲＤ無し＃" UPON CONS
         GO                  TO   INIT-EXIT
     END-IF.
*
 INIT-EXIT.
     EXIT.
*
****************************************************************
*             メイン処理                    2.0                *
****************************************************************
 MAIN-SEC               SECTION.
*
     MOVE    "MAIN-SEC"           TO    S-NAME.
*テクノス連携ファイル出力
     MOVE     SPACE               TO    SND-REC.
     INITIALIZE                         SND-REC.
*項目セット
*管理番号
     MOVE     JH1-F01             TO    SND-F01.
*バッチ日付
     MOVE     JH1-F02             TO    SND-F02.
*バッチ時刻
     MOVE     JH1-F03             TO    SND-F03.
*バッチ取引先
     MOVE     JH1-F04             TO    SND-F04.
*倉庫ＣＤ
     MOVE     JH1-F05             TO    SND-F05.
*店舗ＣＤ
     MOVE     JH1-F06             TO    SND-F06.
*納品場所
     MOVE     JH1-HE20            TO    SND-F07.
*納品日　
     MOVE     JH1-F09             TO    SND-F08.
*出荷日
     MOVE     ZERO                TO    SND-F09.
*入荷予定日
     MOVE     ZERO                TO    SND-F10.
*伝票番号
     MOVE     JH1-F07             TO    SND-F11.
*行番号
     MOVE     JH1-F08             TO    SND-F12.
*商品ＣＤ（インストア）
     MOVE     JH1-F13             TO    SND-F13.
*ＪＡＮＣＤ
     MOVE     JH1-ME04            TO    SND-F14.
*納品数
     MOVE     JH1-F20             TO    SND-F15.
*訂正区分
     MOVE     JH1-F23             TO    SND-F16.
*発注形態
     MOVE     "00"                TO    SND-F17.
*テクノス連携ファイル出力
     WRITE    SND-REC.
*
     ADD      1                   TO    WT-CNT.
*次レコード読込み
     PERFORM  NFJOHOL1-RD-SEC.
*
 MAIN-EXIT.
     EXIT.
*
***************************************************************
*             終了処理                      3.0               *
***************************************************************
 END-SEC                SECTION.
*
     MOVE    "END-SEC"  TO        S-NAME.
*
     CLOSE    NFJOHOL1 SNDTECNS.
*
     DISPLAY "READ-CNT  = " RD-CNT UPON CONS.
     DISPLAY "WRITE-CNT = " WT-CNT UPON CONS.
*
 END-EXIT.
     EXIT.
*
****************************************************************
*    ナフコ　基本情報ファイル　管理_用　　
****************************************************************
 NFJOHOL1-RD-SEC            SECTION.
*
     MOVE    "NFJOHOL1-RD-SEC"    TO   S-NAME.
*
     READ     NFJOHOL1
          AT END
              MOVE     "END"      TO   END-FLG
              GO     TO    NFJOHOL1-RD-EXIT
     END-READ.
 NFJOHOL1-010.
     ADD      1                   TO   RD-CNT.
 NFJOHOL1-020.
*管理_のチェック
     IF   JH1-F01   >   PARA-KANRINO
          MOVE   "END"            TO   END-FLG
          GO     TO    NFJOHOL1-RD-EXIT
     END-IF.
*
 NFJOHOL1-RD-EXIT.
     EXIT.
*

```
