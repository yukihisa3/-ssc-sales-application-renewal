# SSY5180B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SSY5180B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　ナフコ出荷支援                    *
*    モジュール名　　　　：　発注確認　店舗ワーク作成          *
*    作成日／更新日　　　：　2019/02/20                        *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　パラメタ情報に合致する箱数ファイル*
*    　　　　　　　　　　　　レコードより、発注書店舗ワークを　*
*                          　作成する。　　　　　　　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SSY5180B.
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
     SELECT   NFHAKOL7  ASSIGN    TO        DA-01-VI-NFHAKOL7
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   SEQUENTIAL
                        RECORD    KEY       IS   HAK-F01
                                                 HAK-F08
                                                 HAK-F06
                                                 HAK-F05
                                                 HAK-F07
                        FILE      STATUS    IS   HAK-STATUS.
****<< ナフコ店舗マスタ >>************************************
     SELECT   NFTENMS1  ASSIGN    TO        DA-01-VI-NFTENMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   TEN-F01
                                                 TEN-F02
                        FILE      STATUS    IS   TEN-STATUS.
****<< 発注確認店舗ワーク >>********************************
     SELECT   MEITXXXF  ASSIGN    TO        DA-01-VS-MEITXXXF
                        ORGANIZATION        IS   SEQUENTIAL
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   MEI-STATUS.
***************************************************************
 DATA                   DIVISION.
***************************************************************
 FILE                   SECTION.
***************************************************************
****<< 箱数ファイル >>***********************************
 FD  NFHAKOL7.
     COPY     NFHAKOL7  OF        XFDLIB
              JOINING   HAK       PREFIX.
****<< ナフコ店舗マスタ >>*********************************
 FD  NFTENMS1.
     COPY     NFTENMS1  OF        XFDLIB
              JOINING   TEN       PREFIX.
****<< 発注書店舗ワーク >>*************************************
 FD  MEITXXXF.
     COPY     MEITXXXF  OF        XFDLIB
              JOINING   MEI       PREFIX.
****  作業領域  ************************************************
 WORKING-STORAGE        SECTION.
****************************************************************
****  ステイタス情報          ****
 01  STATUS-AREA.
     02 HAK-STATUS           PIC  X(02).
     02 TEN-STATUS           PIC  X(02).
     02 MEI-STATUS           PIC  X(02).
**** メッセージ情報           ****
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SSY5180B".
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
         05  ST-PG          PIC   X(08)  VALUE "SSY5180B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
****  フラグ                  ****
 01  END-FLG                 PIC  X(03)  VALUE  SPACE.
 01  TENPO-END               PIC  X(03)  VALUE  SPACE.
 01  NFTENMS1-INV-FLG        PIC  X(03)  VALUE  SPACE.
 01  MEITXXXF-INV-FLG        PIC  X(03)  VALUE  SPACE.
 01  IX                      PIC  9(03)  VALUE  ZERO.
 01  TBL-FLG                 PIC  9(01)  VALUE  ZERO.
****  カウント                ****
 01  CNT-AREA.
     03  HAK-CNT             PIC  9(07)   VALUE  0.
     03  MEI-CNT             PIC  9(07)   VALUE  0.
 01  CNT-TBLNO               PIC  9(03)   VALUE  0.
 01  CNT-PAGENO              PIC  9(05)   VALUE  0.
 01  CNT-PAGESEQ             PIC  9(02)   VALUE  0.
****  ブレイクキー            ****
 01  BRK-AREA.
     03  OLD-HAK-F08         PIC  9(08)   VALUE  0.
     03  OLD-HAK-F06         PIC  9(05)   VALUE  0.
     03  OLD-HAK-F07         PIC  X(01)   VALUE  SPACE.
*    03  OLD-HAK-F05         PIC  X(02)   VALUE  SPACE.
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
 01  LINK-IN-KANRINO    PIC  9(08).
*01  LINK-IN-SAKUBACD   PIC  X(02).
 01  LINK-IN-SAKUBACD.
     03  LINK-IN-SAKU-01 PIC  X(02).
     03  LINK-IN-SAKU-02 PIC  X(02).
     03  LINK-IN-SAKU-03 PIC  X(02).
     03  LINK-IN-SAKU-04 PIC  X(02).
     03  LINK-IN-SAKU-05 PIC  X(02).
     03  LINK-IN-SAKU-06 PIC  X(02).
     03  LINK-IN-SAKU-07 PIC  X(02).
     03  LINK-IN-SAKU-08 PIC  X(02).
     03  LINK-IN-SAKU-09 PIC  X(02).
     03  LINK-IN-SAKU-10 PIC  X(02).
     03  LINK-IN-SAKU-11 PIC  X(02).
     03  LINK-IN-SAKU-12 PIC  X(02).
     03  LINK-IN-SAKU-13 PIC  X(02).
     03  LINK-IN-SAKU-14 PIC  X(02).
     03  LINK-IN-SAKU-15 PIC  X(02).
     03  LINK-IN-SAKU-16 PIC  X(02).
     03  LINK-IN-SAKU-17 PIC  X(02).
     03  LINK-IN-SAKU-18 PIC  X(02).
     03  LINK-IN-SAKU-19 PIC  X(02).
     03  LINK-IN-SAKU-20 PIC  X(02).
************************************************************
 PROCEDURE              DIVISION       USING  LINK-IN-KANRINO
                                              LINK-IN-SAKUBACD.
************************************************************
 DECLARATIVES.
***
 FILEERR-SEC1           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  NFHAKOL7.
     MOVE   "NFHAKOL7"        TO    ERR-FL-ID.
     MOVE    HAK-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC2           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  NFTENMS1.
     MOVE   "NFTENMS1 "         TO    ERR-FL-ID.
     MOVE    TEN-STATUS       TO    ERR-STCD.
     DISPLAY MSG-ABEND1       UPON  CONS.
     DISPLAY MSG-ABEND2       UPON  CONS.
     MOVE    4000             TO    PROGRAM-STATUS.
     STOP     RUN.
***
 FILEERR-SEC3           SECTION.
     USE AFTER     EXCEPTION
                   PROCEDURE  MEITXXXF.
     MOVE   "MEITXXXF"        TO    ERR-FL-ID.
     MOVE    MEI-STATUS       TO    ERR-STCD.
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
     OPEN         INPUT     NFHAKOL7.
     OPEN         INPUT     NFTENMS1.
     OPEN         OUTPUT    MEITXXXF.
*
     DISPLAY  MSG-START UPON CONS.
*システム時刻取得
     ACCEPT   SYS-TIME  FROM      TIME.
*システム日付取得
     ACCEPT   SYSYMD    FROM      DATE.
*
     PERFORM NFHAKOL7-START-SEC.
     IF     END-FLG  =  "END"
            DISPLAY NC"＃＃　作成対象データ無１　＃＃" UPON CONS
            MOVE      4010       TO   PROGRAM-STATUS
            GO   TO   100-INIT-END
     END-IF.
*
     PERFORM NFHAKOL7-READ-SEC.
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
 NFHAKOL7-START-SEC   SECTION.
*
*    　　　　箱数ファイルスタート
     MOVE        SPACE         TO  HAK-REC.
     INITIALIZE                    HAK-REC.
*
     MOVE   LINK-IN-KANRINO    TO  HAK-F01.
*    MOVE   LINK-IN-SAKUBACD   TO  HAK-F05.
     START  NFHAKOL7  KEY  IS  >=  HAK-F01  HAK-F08  HAK-F06
                                   HAK-F05  HAK-F07
            INVALID
            MOVE   "END"        TO   END-FLG
     END-START.
*
 NFHAKOL7-START-EXIT.
     EXIT.
************************************************************
*    　　　箱数ファイルデータ読込み
************************************************************
 NFHAKOL7-READ-SEC                 SECTION.
*
     READ    NFHAKOL7
             AT   END
             MOVE      "END"     TO   END-FLG
             GO        TO        NFHAKOL7-READ-EXIT
     END-READ.
*トレー数が０の場合は、納品数＝ゼロの為、対象外
     IF      HAK-F11  >  ZERO
             CONTINUE
     ELSE
             GO        TO        NFHAKOL7-READ-SEC
     END-IF.
*管理番号ブレイクで終了
     IF      HAK-F01 > LINK-IN-KANRINO
             MOVE      "END"     TO   END-FLG
             GO        TO        NFHAKOL7-READ-EXIT
     END-IF.
*
*同一店舗は（作場は異なっていても）読み飛ばし
     IF    ( CNT-TBLNO   =   ZERO        ) OR
           ( HAK-F08 NOT =   OLD-HAK-F08   OR
             HAK-F06 NOT =   OLD-HAK-F06 )
             CONTINUE
     ELSE
             GO        TO        NFHAKOL7-READ-SEC
     END-IF.
*
*    IF      LINK-IN-SAKUBACD  = SPACE
*            CONTINUE
*    ELSE
*        IF  HAK-F05  =  LINK-IN-SAKUBACD
*            CONTINUE
*        ELSE
*            MOVE      "END"     TO   END-FLG
*            GO        TO        NFHAKOL7-READ-EXIT
*        END-IF
*    END-IF.
*
     EVALUATE  HAK-F05
       WHEN    LINK-IN-SAKU-01
               CONTINUE
       WHEN    LINK-IN-SAKU-02
               CONTINUE
       WHEN    LINK-IN-SAKU-03
               CONTINUE
       WHEN    LINK-IN-SAKU-04
               CONTINUE
       WHEN    LINK-IN-SAKU-05
               CONTINUE
       WHEN    LINK-IN-SAKU-06
               CONTINUE
       WHEN    LINK-IN-SAKU-07
               CONTINUE
       WHEN    LINK-IN-SAKU-08
               CONTINUE
       WHEN    LINK-IN-SAKU-09
               CONTINUE
       WHEN    LINK-IN-SAKU-10
               CONTINUE
       WHEN    LINK-IN-SAKU-11
               CONTINUE
       WHEN    LINK-IN-SAKU-12
               CONTINUE
       WHEN    LINK-IN-SAKU-13
               CONTINUE
       WHEN    LINK-IN-SAKU-14
               CONTINUE
       WHEN    LINK-IN-SAKU-15
               CONTINUE
       WHEN    LINK-IN-SAKU-16
               CONTINUE
       WHEN    LINK-IN-SAKU-17
               CONTINUE
       WHEN    LINK-IN-SAKU-18
               CONTINUE
       WHEN    LINK-IN-SAKU-19
               CONTINUE
       WHEN    LINK-IN-SAKU-20
               CONTINUE
       WHEN    OTHER
               GO   TO    NFHAKOL7-READ-SEC
     END-EVALUATE.
*
     ADD     1         TO        HAK-CNT.
*
 NFHAKOL7-READ-EXIT.
     EXIT.
************************************************************
*      ２００   主処理　                                   *
************************************************************
 200-MAIN-SEC           SECTION.
*
     MOVE  SPACE     TO  MEI-REC.
     INITIALIZE          MEI-REC.
*管理番号
     MOVE  HAK-F01   TO  MEI-F01.
*バッチ日付
     MOVE  HAK-F02   TO  MEI-F02.
*バッチ時刻
     MOVE  HAK-F03   TO  MEI-F03.
*取引先ＣＤ
     MOVE  HAK-F04   TO  MEI-F04.
*作場ＣＤ
*****MOVE  HAK-F05   TO  MEI-F05.
*店舗ＣＤ
     MOVE  HAK-F06   TO  MEI-F06.
*納品場所
     MOVE  HAK-F07   TO  MEI-F07.
*店着日
     MOVE  HAK-F08   TO  MEI-F09.
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
              MOVE    LINK-OUT-YMD8    TO   MEI-F08
     ELSE
              DISPLAY NC"出荷日算出エラー！"      UPON CONS
              DISPLAY NC"算出元店着日＝" HAK-F08  UPON CONS
              DISPLAY NC"エラーＣＤ　＝" LINK-OUT-RET UPON CONS
              MOVE    4010             TO   PROGRAM-STATUS
              STOP    RUN
     END-IF.
*店舗名
     PERFORM  NFTENMS1-READ-SEC.
     IF  NFTENMS1-INV-FLG = "INV"
         MOVE  ALL NC"＊"   TO  MEI-F10
     ELSE
*        MOVE  TEN-F03      TO  MEI-F10
         MOVE  TEN-F05      TO  MEI-F10
     END-IF.
*箱数
     MOVE  HAK-F09     TO  MEI-F11.
*アイテム数
     MOVE  HAK-F10     TO  MEI-F12.
*ＴＢＬＮＯ
     IF  ( CNT-TBLNO   =   ZERO        ) OR
***      ( HAK-F05 NOT =   OLD-HAK-F05 ) OR
         ( HAK-F08 NOT =   OLD-HAK-F08 )
*↓店着日ブレイクでTBLNOを1に戻さない
*---       MOVE  1     TO  CNT-TBLNO
           ADD   1     TO  CNT-TBLNO
*↑
     ELSE
           ADD   1     TO  CNT-TBLNO
     END-IF.
     MOVE  CNT-TBLNO   TO  MEI-F13.
*頁ＮＯ
     DIVIDE CNT-TBLNO  BY  12  GIVING    SYO
                               REMAINDER AMARI.
     IF  ( CNT-PAGENO  =   ZERO        ) OR
***      ( HAK-F05 NOT =   OLD-HAK-F05 ) OR
         ( HAK-F08 NOT =   OLD-HAK-F08 )
           MOVE  1     TO  CNT-PAGENO
     ELSE
           IF  AMARI   =   1
               ADD     1   TO  CNT-PAGENO
           END-IF
     END-IF.
     MOVE  CNT-PAGENO  TO  MEI-F14.
*頁内ＳＥＱ
*    IF  ( HAK-F05 NOT =   OLD-HAK-F05 ) OR
     IF  ( HAK-F08 NOT =   OLD-HAK-F08 ) OR
         ( CNT-PAGESEQ =   12          )
           MOVE  1     TO  CNT-PAGESEQ
     ELSE
           ADD   1     TO  CNT-PAGESEQ
     END-IF.
     MOVE  CNT-PAGESEQ TO  MEI-F15.
*
     WRITE MEI-REC.

     ADD   1         TO  MEI-CNT.
*
***  MOVE  HAK-F05   TO  OLD-HAK-F05.
     MOVE  HAK-F06   TO  OLD-HAK-F06.
     MOVE  HAK-F08   TO  OLD-HAK-F08.
*
     PERFORM NFHAKOL7-READ-SEC.
*
 200-MAIN-SEC-EXT.
     EXIT.
************************************************************
*            店舗テーブルの読込処理
************************************************************
 NFTENMS1-READ-SEC                   SECTION.
*
*    MOVE    HAK-F04            TO    TEN-F52.
     MOVE    HAK-F04            TO    TEN-F01.
*    MOVE    HAK-F06            TO    TEN-F011.
     MOVE    HAK-F06            TO    TEN-F02.
     READ    NFTENMS1
             INVALID     MOVE  "INV"  TO  NFTENMS1-INV-FLG
             NOT INVALID MOVE  SPACE  TO  NFTENMS1-INV-FLG
     END-READ.
*****DISPLAY "INV   = " NFTENMS1-INV-FLG   UPON CONS.
*
 NFTENMS1-READ-EXT.
     EXIT.
************************************************************
*      ３００     終了処理                                 *
************************************************************
 300-END-SEC           SECTION.
     CLOSE             NFTENMS1   NFHAKOL7  MEITXXXF.
*
     DISPLAY "* NFHAKOL7 (INPUT) = " HAK-CNT  " *"  UPON CONS.
     DISPLAY "* MEITXXXF (WRITE) = " MEI-CNT  " *"  UPON CONS.
*
 300-END-SEC-EXT.
     EXIT.
*****************<<  PROGRAM  END  >>***********************

```
