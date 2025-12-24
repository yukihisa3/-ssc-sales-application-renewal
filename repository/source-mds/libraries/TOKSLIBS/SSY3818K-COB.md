# SSY3818K

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SSY3818K.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　ナフコ出荷支援                    *
*    モジュール名　　　　：　発注書店舗ワーク作成              *
*    作成日／更新日　　　：　2015/05/07                        *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　発注集計Ｆを読み、店舗順Ｆを作成　*
*                          　パラメタ情報に合致する箱数ファイル*
*                          　レコードより、発注書店舗ワークを  *
*                          　作成する。　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSY3818K.
 AUTHOR.                NAV.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       GP6000.
 OBJECT-COMPUTER.       GP6000.
 SPECIAL-NAMES.
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
****<< 箱数ファイル >>**********************************
     SELECT   NFHAKOL6  ASSIGN    TO        DA-01-VI-NFHAKOL6
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   SEQUENTIAL
                        RECORD    KEY       IS   HAK-F01
                                                 HAK-F05
                                                 HAK-F08
                                                 HAK-F06
                                                 HAK-F07
                        FILE      STATUS    IS   HAK-STATUS.
****<< 店舗マスタ >>******************************************
     SELECT   TENMS1    ASSIGN    TO        DA-01-VI-TENMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   TEN-F52
                                                 TEN-F011
                        FILE      STATUS    IS   TEN-STATUS.
****<< 発注書店舗ワーク >>**********************************
     SELECT   HCTPXXXF  ASSIGN    TO        DA-01-VS-HCTPXXXF
                        ORGANIZATION        IS   SEQUENTIAL
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   HCT-STATUS.
***************************************************************
 DATA                   DIVISION.
***************************************************************
 FILE                   SECTION.
***************************************************************
****<< 箱数ファイル >>***********************************
 FD  NFHAKOL6.
     COPY     NFHAKOL6  OF        XFDLIB
              JOINING   HAK       PREFIX.
****<< 店舗マスタ >>***************************************
 FD  TENMS1.
     COPY     TENMS1    OF        XFDLIB
              JOINING   TEN       PREFIX.
****<< 発注書店舗ワーク >>*************************************
 FD  HCTPXXXF.
     COPY     HCTPXXXF  OF        XFDLIB
              JOINING   HCT       PREFIX.
****  作業領域  ************************************************
 WORKING-STORAGE        SECTION.
****************************************************************
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 HAK-STATUS           PIC  X(02).
     02 TEN-STATUS           PIC  X(02).
     02 HCT-STATUS           PIC  X(02).
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SSY3818K".
       03  FILLER            PIC  X(10)  VALUE
          " ABEND ###".
     02  MSG-ABEND2.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-FL-ID         PIC  X(08).
       03  FILLER            PIC  X(04)  VALUE  " ST-".
       03  ERR-STCD          PIC  X(02).
       03  FILLER            PIC  X(04)  VALUE  " ###".
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SSY3818K".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
****  フラグ                  ****
 01  END-FLG                 PIC  X(03)  VALUE  SPACE.
 01  TENPO-END               PIC  X(03)  VALUE  SPACE.
 01  TENMS1-INV-FLG          PIC  X(03)  VALUE  SPACE.
 01  HCTPXXXF-INV-FLG        PIC  X(03)  VALUE  SPACE.
 01  IX                      PIC  9(03)  VALUE  ZERO.
 01  TBL-FLG                 PIC  9(01)  VALUE  ZERO.
****  カウント                ****
 01  CNT-AREA.
     03  HAK-CNT             PIC  9(07)   VALUE  0.
     03  HCT-CNT             PIC  9(07)   VALUE  0.
 01  CNT-TBLNO               PIC  9(03)   VALUE  0.
 01  CNT-PAGENO              PIC  9(05)   VALUE  0.
 01  CNT-PAGESEQ             PIC  9(02)   VALUE  0.
****  ブレイクキー            ****
 01  BRK-AREA.
     03  OLD-HAK-F05         PIC  X(02)   VALUE  SPACE.
     03  OLD-HAK-F08         PIC  9(08)   VALUE  0.
****  計算ワーク            ****
 01  CAL-AREA.
     03  SYO                 PIC  9(08)   VALUE  0.
     03  AMARI               PIC  9(08)   VALUE  0.
****                        ****
***** システム日付ワーク
 01  SYSTEM-HIZUKE.
     03  SYSYMD              PIC   9(06)  VALUE  ZERO.
     03  SYS-DATEW           PIC   9(08)  VALUE  ZERO.
     03  SYS-DATE-R          REDEFINES SYS-DATEW.
         05  SYS-YY          PIC   9(04).
         05  SYS-MM          PIC   9(02).
         05  SYS-DD          PIC   9(02).
***** システム時刻ワーク
 01  SYS-TIME           PIC  9(08).
 01  FILLER             REDEFINES      SYS-TIME.
     03  SYS-HHMMSS     PIC  9(06).
     03  SYS-MS         PIC  9(02).
*
*    日付変換ワーク（パラメタ用）
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
************************************************************
 LINKAGE            SECTION.
*01  LINK-TOKCD1        PIC  9(08).
 01  LINK-IN-KANRINO    PIC  9(08).
 01  LINK-IN-SAKUBACD   PIC  X(02).
************************************************************
*PROCEDURE              DIVISION       USING  LINK-TOKCD1.
 PROCEDURE              DIVISION       USING  LINK-IN-KANRINO
                                              LINK-IN-SAKUBACD.
************************************************************
 DECLARATIVES.
***
 FILEERR-SEC1           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  NFHAKOL6.
     MOVE   "NFHAKOL6"        TO    ERR-FL-ID.
     MOVE    HAK-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC2           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  TENMS1.
     MOVE   "TENMS1 "         TO    ERR-FL-ID.
     MOVE    TEN-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC3           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  HCTPXXXF.
     MOVE   "HCTPXXXF"        TO    ERR-FL-ID.
     MOVE    HCT-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 END     DECLARATIVES.
************************************************************
*             基本処理
************************************************************
 PGM-CONTROL                     SECTION.
     PERFORM           100-INIT-SEC.
     PERFORM           200-MAIN-SEC
             UNTIL     END-FLG   =    "END".
     PERFORM           300-END-SEC.
     STOP     RUN.
 PGM-CONTROL-EXT.
     EXIT.
************************************************************
*               初期処理                                   *
************************************************************
 100-INIT-SEC           SECTION.
*
     OPEN         INPUT     NFHAKOL6.
     OPEN         INPUT     TENMS1.
     OPEN         OUTPUT    HCTPXXXF.
*
     DISPLAY  MSG-START UPON CONS.
*システム時刻取得
     ACCEPT   SYS-TIME  FROM      TIME.
*システム日付取得
     ACCEPT   SYSYMD    FROM      DATE.
*
     PERFORM NFHAKOL6-START-SEC.
     IF     END-FLG  =  "END"
            DISPLAY NC"＃＃　作成対象データ無１　＃＃" UPON CONS
            MOVE      4010       TO   PROGRAM-STATUS
            GO   TO   100-INIT-END
     END-IF.
*
     PERFORM NFHAKOL6-READ-SEC.
     IF     END-FLG  =  "END"
            DISPLAY NC"＃＃　作成対象データ無２　＃＃" UPON CONS
            MOVE      4010       TO   PROGRAM-STATUS
            GO   TO   100-INIT-END
     END-IF.
*
 100-INIT-END.
     EXIT.
****************************************************************
*　　　　　　箱数ファイルスタート
****************************************************************
 NFHAKOL6-START-SEC   SECTION.
*
*    　　　　箱数ファイルスタート
     MOVE        SPACE         TO  HAK-REC.
     INITIALIZE                    HAK-REC.
*
     MOVE   LINK-IN-KANRINO    TO  HAK-F01.
     MOVE   LINK-IN-SAKUBACD   TO  HAK-F05.
     START  NFHAKOL6  KEY  IS  >=  HAK-F01  HAK-F05  HAK-F08
                                   HAK-F06  HAK-F07
            INVALID
            MOVE   "END"        TO   END-FLG
     END-START.
*
 NFHAKOL6-START-EXIT.
     EXIT.
************************************************************
*    　　　箱数ファイルデータ読込み
************************************************************
 NFHAKOL6-READ-SEC                 SECTION.
*
     READ    NFHAKOL6
             AT   END
             MOVE      "END"     TO   END-FLG
             GO        TO        NFHAKOL6-READ-EXIT
     END-READ.
*2015/10/01 NAV ST
*トレー数が０の場合は、納品数＝ゼロの為、対象外
     IF      HAK-F11  >  ZERO
             CONTINUE
     ELSE
             GO        TO        NFHAKOL6-READ-SEC
     END-IF.
*2015/10/01 NAV ED
*
     IF      HAK-F01 > LINK-IN-KANRINO
             MOVE      "END"     TO   END-FLG
             GO        TO        NFHAKOL6-READ-EXIT
     END-IF.
*
     IF      LINK-IN-SAKUBACD  = SPACE
             CONTINUE
     ELSE
         IF  HAK-F05  =  LINK-IN-SAKUBACD
             CONTINUE
         ELSE
             MOVE      "END"     TO   END-FLG
             GO        TO        NFHAKOL6-READ-EXIT
         END-IF
     END-IF.
*
     ADD     1         TO        HAK-CNT.
*
 NFHAKOL6-READ-EXIT.
     EXIT.
************************************************************
*      ２００   主処理　                                   *
************************************************************
 200-MAIN-SEC           SECTION.
*
     MOVE  SPACE     TO  HCT-REC.
     INITIALIZE          HCT-REC.
*管理番号
     MOVE  HAK-F01   TO  HCT-F01.
*バッチ日付
     MOVE  HAK-F02   TO  HCT-F02.
*バッチ時刻
     MOVE  HAK-F03   TO  HCT-F03.
*取引先ＣＤ
     MOVE  HAK-F04   TO  HCT-F04.
*作場ＣＤ
     MOVE  HAK-F05   TO  HCT-F05.
*店舗ＣＤ
     MOVE  HAK-F06   TO  HCT-F06.
*納品場所
     MOVE  HAK-F07   TO  HCT-F07.
*店着日
     MOVE  HAK-F08   TO  HCT-F09.
*出荷日
*店着日－１日
     MOVE    "6"             TO   LINK-IN-KBN.
     MOVE     1              TO   LINK-IN-YMD6.
     MOVE     HAK-F08        TO   LINK-IN-YMD8.
     CALL    "SKYDTCKB" USING     LINK-IN-KBN
                                  LINK-IN-YMD6
                                  LINK-IN-YMD8
                                  LINK-OUT-RET
                                  LINK-OUT-YMD8.
     IF       LINK-OUT-RET   =    ZERO
              MOVE    LINK-OUT-YMD8    TO   HCT-F08
     ELSE
              DISPLAY NC"出荷日算出エラー！"      UPON CONS
              DISPLAY NC"算出元店着日＝" HAK-F08  UPON CONS
              DISPLAY NC"エラーＣＤ　＝" LINK-OUT-RET UPON CONS
              MOVE    4010             TO   PROGRAM-STATUS
              STOP    RUN
     END-IF.
*店舗名
     PERFORM  TENMS1-READ-SEC.
     IF  TENMS1-INV-FLG = "INV"
         MOVE  ALL NC"＊"   TO  HCT-F10
     ELSE
         MOVE  TEN-F03      TO  HCT-F10
     END-IF.
*箱数
     MOVE  HAK-F09     TO  HCT-F11.
*アイテム数
     MOVE  HAK-F10     TO  HCT-F12.
*ＴＢＬＮＯ
     IF  ( CNT-TBLNO   =   ZERO        ) OR
         ( HAK-F05 NOT =   OLD-HAK-F05 ) OR
         ( HAK-F08 NOT =   OLD-HAK-F08 )
           MOVE  1     TO  CNT-TBLNO
     ELSE
           ADD   1     TO  CNT-TBLNO
     END-IF.
     MOVE  CNT-TBLNO   TO  HCT-F13.
*頁ＮＯ
     DIVIDE CNT-TBLNO  BY  12  GIVING    SYO
                               REMAINDER AMARI.
     IF  ( CNT-PAGENO  =   ZERO        ) OR
         ( HAK-F05 NOT =   OLD-HAK-F05 ) OR
         ( HAK-F08 NOT =   OLD-HAK-F08 )
           MOVE  1     TO  CNT-PAGENO
     ELSE
           IF  AMARI   =   1
               ADD     1   TO  CNT-PAGENO
           END-IF
     END-IF.
     MOVE  CNT-PAGENO  TO  HCT-F14.
*頁内ＳＥＱ
     IF  ( HAK-F05 NOT =   OLD-HAK-F05 ) OR
         ( HAK-F08 NOT =   OLD-HAK-F08 ) OR
         ( CNT-PAGESEQ =   12          )
           MOVE  1     TO  CNT-PAGESEQ
     ELSE
           ADD   1     TO  CNT-PAGESEQ
     END-IF.
     MOVE  CNT-PAGESEQ TO  HCT-F15.
*
     WRITE HCT-REC.

     ADD   1         TO  HCT-CNT.
*
     MOVE  HAK-F05   TO  OLD-HAK-F05.
     MOVE  HAK-F08   TO  OLD-HAK-F08.
*
     PERFORM NFHAKOL6-READ-SEC.
*
 200-MAIN-SEC-EXT.
     EXIT.
************************************************************
*            店舗テーブルの読込処理
************************************************************
 TENMS1-READ-SEC                   SECTION.
*
*    MOVE    LINK-TOKCD1        TO    TEN-F52.
     MOVE    HAK-F04            TO    TEN-F52.
     MOVE    HAK-F06            TO    TEN-F011.
     READ    TENMS1
             INVALID     MOVE  "INV"  TO  TENMS1-INV-FLG
             NOT INVALID MOVE  SPACE  TO  TENMS1-INV-FLG
     END-READ.
*****DISPLAY "INV   = " TENMS1-INV-FLG   UPON CONS.
*
 TENMS1-READ-EXT.
     EXIT.
************************************************************
*    ＣＳＶ商品ファイル読込み
************************************************************
*HCTPXXXF-READ-SEC                  SECTION.
*
*    MOVE    HAK-F06             TO   HCT-F01.
*    READ    HCTPXXXF
*            INVALID     MOVE "INV" TO HCTPXXXF-INV-FLG
*            NOT INVALID MOVE SPACE TO HCTPXXXF-INV-FLG
*    END-READ.
*
*HCTPXXXF-READ-EXIT.
*    EXIT.
************************************************************
*      ３００     終了処理                                 *
************************************************************
 300-END-SEC           SECTION.
     CLOSE             TENMS1   NFHAKOL6  HCTPXXXF.
*
     DISPLAY "* NFHAKOL6 (INPUT) = " HAK-CNT  " *"  UPON CONS.
     DISPLAY "* HCTPXXXF (WRITE) = " HCT-CNT  " *"  UPON CONS.
*
 300-END-SEC-EXT.
     EXIT.
*****************<<  PROGRAM  END  >>***********************

```
