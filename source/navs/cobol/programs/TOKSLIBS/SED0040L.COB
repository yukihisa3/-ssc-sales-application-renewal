****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ　　　　　　　　*
*    業務名　　　　　　　：　ＥＤＩＣ　　　　　　　　　　　　　*
*    モジュール名　　　　：　不照合リスト発行　　　　　　　　　*
*    作成日／更新日　　　：　2015/09/01                        *
*    作成者／更新者　　　：　ＮＡＶ宮下　　　　　　　　　　　　*
*    処理概要　　　　　　：　ＥＤＩＣ不照合リストワークより　　*
*                        ：　ＥＤＩＣ不照合リストを出力する。　*
*    更新日／更新者　　　：　                                  *
*    更新概要　　　　　　：　　　　　　　　　　　　　　　　　　*
*                                                              *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SED0040L.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2015/09/01.
 DATE-COMPILED.
 SECURITY.              NONE.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM.
 OBJECT-COMPUTER.       FACOM.
 SPECIAL-NAMES.
*    STATION     IS        STA
     YA          IS        NIHONGO
     YB          IS        YB
     YB-21       IS        YB-21
     YB-22       IS        YB-22
     CONSOLE     IS        CONS.
*--------------------------------------------------------------*
 INPUT-OUTPUT           SECTION.
*--------------------------------------------------------------*
 FILE-CONTROL.
*----<< EDIC不照合リストワーク >>--*
     SELECT   EWFUSHF   ASSIGN    TO     DA-01-VI-EWFUSHL1
                        ORGANIZATION     INDEXED
                        ACCESS    MODE   SEQUENTIAL
                        RECORD    KEY    FWK-F01
                                         FWK-F05
                                         FWK-F18
                                         FWK-F04
                        STATUS           FWK-ST.
*----<< プリンタ >>-*
     SELECT   PRTF      ASSIGN    TO     LP-04-PRTF.
*
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< EDIC不照合リストワーク >>--*
 FD  EWFUSHF            LABEL RECORD   IS   STANDARD.
     COPY     EWFUSHF   OF        XFDLIB
              JOINING   FWK       PREFIX.
*----<< プリンタ >>-*
 FD  PRTF               LABEL RECORD   IS   OMITTED.
 01  PRT-REC            PIC  X(200).
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
 01  FLAGS.
     03  END-FLG        PIC  X(03).
     03  INV-FLG1       PIC  9(01).
     03  INV-FLG2       PIC  9(01).
 01  TEN-FLG            PIC  9(01)  VALUE 0.
 01  COUNTERS.
     03  LINE-CNT       PIC  9(03).
     03  PAGE-CNT       PIC  9(03).
     03  CNT-SEI        PIC  9(06).
     03  CNT-SIH        PIC  9(06).
     03  CNT-UNMACTH    PIC  9(06).
     03  CNT-MACTH      PIC  9(06).
     03  CNT-RDSEI      PIC  9(06).
     03  CNT-RDSIH      PIC  9(06).
 01  IDX.
     03  I              PIC  9(03).
*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  FWK-ST             PIC  X(02).
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
*----<< 店舗コード変換 >>--*
 01  TEMPOCD-MOJI           PIC   X(05).
 01  FILLER            REDEFINES      TEMPOCD-MOJI.
     03  TEMPOCD-SUCHI      PIC  9(05).
*
*----<< 伝票番号変換 >>--*
 01  DEMPYONO-MOJI          PIC   X(10).
 01  FILLER            REDEFINES      DEMPYONO-MOJI.
     03  DEMPYONO-SUCHI     PIC   9(10).
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
 01  DEN-FLG            PIC  9(01) VALUE  0.
*
*----<< 印刷制御用変数 >>--*
 01  NEW-TENCD          PIC  X(05)  VALUE  SPACE.
 01  OLD-TENCD          PIC  X(05)  VALUE  SPACE.
*
*----<< 帳票出力定義エリア >>-*
 01  MIDASI-1.
     02  FILLER              PIC  X(01)  VALUE  SPACE.
     02  FILLER              PIC  X(08)  VALUE  "SED0040L".
     02  FILLER              PIC  X(43)  VALUE  SPACE.
     02  FILLER              PIC  N(08)
         CHARACTER  TYPE  IS  YB-22 VALUE  NC"＜不照合リスト＞".
     02  FILLER              PIC  X(24)  VALUE  SPACE.
     02  SYSYY               PIC  9999.
     02  FILLER              PIC  N(01)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"年".
     02  SYSMM               PIC  99.
     02  FILLER              PIC  N(01)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"月".
     02  SYSDD               PIC  99.
     02  FILLER              PIC  N(01)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"日".
     02  FILLER              PIC  X(02)  VALUE  SPACE.
     02  LPAGE               PIC  ZZ9.
     02  FILLER              PIC  N(01)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"頁".
*
****  見出し行２***
 01  MIDASI-2.
     02  FILLER         PIC  X(01)  VALUE  SPACE.
     02  FILLER         PIC  N(07)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"照合請求締日：".
     02  SIMEBI         PIC  9999/99/99.
*
****  見出し行３***
 01  MIDASI-3.
     02  FILLER         PIC  X(07)  VALUE  SPACE.
     02  FILLER         PIC  N(04)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"取引先：".
     02  TORICD         PIC  9(08).
     02  FILLER         PIC  X(01)  VALUE  SPACE.
     02  TORINAM        PIC  N(15)
         CHARACTER  TYPE  IS  NIHONGO.
*
****  見出し行４
 01  MIDASI-4         CHARACTER TYPE  NIHONGO.
     02  FILLER         PIC  X(01)  VALUE  SPACE.
     02  FILLER         PIC  N(04)  VALUE  NC"店舗情報".
     02  FILLER         PIC  X(20)  VALUE  SPACE.
     02  FILLER         PIC  N(04)  VALUE  NC"伝票区分".
     02  FILLER         PIC  X(09)  VALUE  SPACE.
     02  FILLER         PIC  N(04)  VALUE  NC"伝票番号".
     02  FILLER         PIC  X(05)  VALUE  SPACE.
     02  FILLER         PIC  N(04)  VALUE  NC"請求金額".
     02  FILLER         PIC  X(04)  VALUE  SPACE.
     02  FILLER         PIC  N(04)  VALUE  NC"支払金額".
     02  FILLER         PIC  X(02)  VALUE  SPACE.
     02  FILLER         PIC  N(08)  VALUE
                                    NC"レコード区分内容".
     02  FILLER         PIC  X(06)  VALUE  SPACE.
     02  FILLER         PIC  N(05)  VALUE  NC"データ内容".
*
****  線１
 01  HASEN-1.
     02  FILLER         PIC  X(136) VALUE  ALL "=".
*
****  明細行１
 01  MEISAI-1.
     02  FILLER         PIC  X(01)  VALUE  SPACE.
     02  TENCD          PIC  ZZZZ9.
     02  FILLER         PIC  X(01)  VALUE  SPACE.
     02  TENNM          PIC  N(10)
         CHARACTER   TYPE  IS  NIHONGO.
     02  FILLER         PIC  X(02)  VALUE  SPACE.
     02  DENKBN         PIC  X(04).
     02  FILLER         PIC  X(01)  VALUE  SPACE.
     02  DENKBNM        PIC  N(05)
         CHARACTER   TYPE  IS  NIHONGO.
     02  FILLER         PIC  X(02)  VALUE  SPACE.
     02  DENNO          PIC  9(09).
     02  FILLER         PIC  X(01)  VALUE  SPACE.
     02  SEIKYU         PIC  ---,---,--9.
     02  FILLER         PIC  X(01)  VALUE  SPACE.
     02  SIHARAI        PIC  ---,---,--9.
     02  FILLER         PIC  X(02)  VALUE  SPACE.
     02  RNAIYO         PIC  N(10)
         CHARACTER   TYPE  IS  NIHONGO.
     02  FILLER         PIC  X(02)  VALUE  SPACE.
     02  DNAIYO         PIC  N(10)
         CHARACTER   TYPE  IS  NIHONGO.
*
****  総合計
 01 GOKEI-1.
     02  FILLER         PIC  X(02)   VALUE  SPACE.
     02  FILLER         PIC  N(07)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"＜不照合情報＞".
     02  FILLER         PIC  X(02)   VALUE  SPACE.
     02  FILLER         PIC  N(07)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"請求データ無：".
     02  SEIKYU-D       PIC  ---,--9.
     02  FILLER         PIC  X(01)   VALUE  SPACE.
     02  FILLER         PIC  N(07)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"支払データ無：".
     02  FILLER         PIC  X(01)   VALUE  SPACE.
     02  SHIHARAI-D     PIC  ---,--9.
     02  FILLER         PIC  X(01)   VALUE  SPACE.
     02  FILLER         PIC  N(05)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"金額違い：".
     02  FILLER         PIC  X(01)   VALUE  SPACE.
     02  KINGAKU-D      PIC  ---,--9.
     02  FILLER         PIC  X(01)   VALUE  SPACE.
     02  FILLER         PIC  N(05)
         CHARACTER  TYPE  IS  NIHONGO    VALUE
         NC"照合件数：".
     02  FILLER         PIC  X(01)   VALUE  SPACE.
     02  SHOGO-D        PIC  ---,--9.
*
****************************************************************
 PROCEDURE              DIVISION.
****************************************************************
*--------------------------------------------------------------*
*                   エラー処理
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 支払合計ファイル >>--*
 FWK-ERR                SECTION.
     USE AFTER     EXCEPTION PROCEDURE      EWFUSHF.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "### SED0040L EWFUSHF ERROR " FWK-ST " "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS " ###"
                                       UPON CONS.
     STOP     RUN.
 END DECLARATIVES.
*--------------------------------------------------------------*
*                  ﾌﾟﾛｸﾞﾗﾑ ｺﾝﾄﾛｰﾙ
*--------------------------------------------------------------*
 000-PROG-CNTL          SECTION.
     PERFORM  100-INIT-RTN.
     PERFORM  200-MAIN-RTN
              UNTIL     END-FLG = "END".
     PERFORM  300-END-RTN.
     STOP RUN.
 000-PROG-CNTL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*                  ｼｮｷ ｼｮﾘ
*--------------------------------------------------------------*
 100-INIT-RTN           SECTION.
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SED0040L START *** "
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
*----<<FILE OPEN >>--*
     OPEN     INPUT     EWFUSHF.
     OPEN     OUTPUT    PRTF.
*
*----<< ﾜｰｸ ｼｮｷｾｯﾄ >>-*
     INITIALIZE         FLAGS.
     INITIALIZE         COUNTERS.
     MOVE     99             TO   LINE-CNT.
     MOVE     1              TO   INV-FLG1
                                  INV-FLG2.
*
*    ワーク読込み
     PERFORM  900-FWK-READ.
 100-INIT-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*                  ＭＡＩＮ処理
*--------------------------------------------------------------*
 200-MAIN-RTN           SECTION.
**   DISPLAY "200-MAIN-RTN"  UPON  CONS.
*
*----<< 明細出力 >>--*
     PERFORM     220-MEISAI-WRT.
*
*----<< ワーク読込 >>--*
     PERFORM     900-FWK-READ.
*
 200-MAIN-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*                  ＥＮＤ処理
*--------------------------------------------------------------*
 300-END-RTN            SECTION.
*    総合計印刷
     IF    PAGE-CNT     >     0
           PERFORM  TAIL-RTN
     END-IF.
*
*----<<FILE CLOSE >>--*
     CLOSE    EWFUSHF.
     CLOSE    PRTF.
*
     DISPLAY "ワーク読込件数　："  CNT-RDSIH    UPON  CONS.
     DISPLAY "請求データ無件数："  CNT-SEI      UPON  CONS.
     DISPLAY "支払データ無件数："  CNT-SIH      UPON  CONS.
     DISPLAY "金額違い　　件数："  CNT-UNMACTH  UPON  CONS.
     DISPLAY "照合件数　　　　："  CNT-MACTH    UPON  CONS.
*
     ACCEPT   SYS-DATE       FROM DATE.
     ACCEPT   SYS-TIME       FROM TIME.
     DISPLAY  "*** SED0040L END   *** "
              SYS-YY "." SYS-MM "." SYS-DD " "
              SYS-HH ":" SYS-MN ":" SYS-SS
                                       UPON CONS.
*****ACCEPT   FWK-ST  FROM   CONS.
*
 300-END-RTN-EXIT.
     EXIT.
*--------------------------------------------------------------*
*                  見出しデータ編集書き出し
*--------------------------------------------------------------*
 210-HEAD-WRT           SECTION.
*
*----<< 改ページチェック >>--*
     IF       PAGE-CNT  >    0
              MOVE    SPACE       TO   PRT-REC
              WRITE   PRT-REC     AFTER  PAGE
     END-IF.
*
*----<< 項目セット >>--*
     ADD      1                   TO   PAGE-CNT.
     MOVE     LINK-OUT-YMD(1:4)   TO   SYSYY.
     MOVE     SYS-MM              TO   SYSMM.
     MOVE     SYS-DD              TO   SYSDD.
     MOVE     PAGE-CNT            TO   LPAGE.
     MOVE     FWK-F01             TO   TORICD.
     MOVE     FWK-F02             TO   TORINAM.
     MOVE     FWK-F03             TO   SIMEBI.
*
*----<< 見出し行出力 >>--*
     WRITE    PRT-REC   FROM      MIDASI-1  AFTER     1.
     WRITE    PRT-REC   FROM      MIDASI-2  AFTER     2.
     WRITE    PRT-REC   FROM      MIDASI-3  AFTER     1.
     WRITE    PRT-REC   FROM      HASEN-1   AFTER     1.
     WRITE    PRT-REC   FROM      MIDASI-4  AFTER     1.
     WRITE    PRT-REC   FROM      HASEN-1   AFTER     1.
     MOVE     8                   TO   LINE-CNT.
     MOVE     0                   TO   TEN-FLG.
 210-HEAD-WRT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*                    明細データ書き出し
*--------------------------------------------------------------*
 220-MEISAI-WRT         SECTION.
*
*----<< フッタ部集計 >>--*
     EVALUATE  FWK-F32
         WHEN  "1"
               ADD  1             TO        CNT-MACTH
         WHEN  "2"
               ADD  1             TO        CNT-SEI
         WHEN  "3"
               ADD  1             TO        CNT-SIH
         WHEN  "4"
               ADD  1             TO        CNT-UNMACTH
     END-EVALUATE.
*
*----<< 改ページチェック >>--*
     IF  LINE-CNT       >         57
         PERFORM    210-HEAD-WRT
     END-IF.
*
*----<< 明細クリア >>--*
     MOVE      SPACE              TO        MEISAI-1.

*----<< 項目セット >>--*
     IF  OLD-TENCD NOT = NEW-TENCD
          MOVE  FWK-F05           TO        TEMPOCD-MOJI
                                            OLD-TENCD
          MOVE  TEMPOCD-SUCHI     TO        TENCD
          MOVE  FWK-F06           TO        TENNM
     END-IF.
     MOVE      FWK-F18            TO        DENKBN.
     MOVE      FWK-F19(1:5)       TO        DENKBNM.
     MOVE      FWK-F04            TO        DENNO.
     MOVE      FWK-F31            TO        SEIKYU.
     MOVE      FWK-F28            TO        SIHARAI.
     MOVE      FWK-F33            TO        RNAIYO.
     MOVE      FWK-F34            TO        DNAIYO.
*
*----<< 明細出力 >>--*
         WRITE     PRT-REC        FROM      MEISAI-1  AFTER  1
         ADD       1              TO        LINE-CNT
*
 220-MEISAI-WRT-EXIT.
     EXIT.
*--------------------------------------------------------------*
*                  総合計書き出し
*--------------------------------------------------------------*
 TAIL-RTN               SECTION.
*
*----<< 改ページチェック >>--*
     IF       LINE-CNT  >    55
              PERFORM   210-HEAD-WRT
     END-IF.
*
*----<< 項目セット >>--*
     MOVE      CNT-SEI            TO        SEIKYU-D.
     MOVE      CNT-SIH            TO        SHIHARAI-D.
     MOVE      CNT-UNMACTH        TO        KINGAKU-D.
     MOVE      CNT-MACTH          TO        SHOGO-D.
*
*----<< 合計行出力 >>--*
     WRITE    PRT-REC   FROM      GOKEI-1   AFTER     2.
*
 TAIL-EXIT.
     EXIT.
*--------------------------------------------------------------*
*             ワーク読込み
*--------------------------------------------------------------*
 900-FWK-READ           SECTION.
**   DISPLAY "900-FWK-READ"  UPON  CONS.
     READ     EWFUSHF
       AT END
              MOVE     "END"           TO   END-FLG
              GO   TO   900-FWK-READ-EXIT
       NOT AT END
              MOVE     FWK-F05         TO   NEW-TENCD
              ADD       1              TO   CNT-RDSIH
     END-READ.
*
 900-FWK-READ-EXIT.
     EXIT.
*-----------------<< PROGRAM END >>----------------------------*
