# SBM0221B

**種別**: COBOL プログラム  
**ライブラリ**: TOKSRLIB  
**ソースファイル**: `source/navs/cobol/programs/TOKSRLIB/SBM0221B.COB`

## ソースコード

```cobol
****************************************************************
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    サブシステム　　　　：　流通ＢＭＳ                        *
*    業務名　　　　　　　：　支払　　　　　　　　　　　　　　　*
*    モジュール名　　　　：　支払データ抽出（ＨＩヒロセ）　　  *
*    作成日／更新日　　　：　2024/06/25                        *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　流通ＢＭＳ支払メッセージより　　　*
*                            支払ワークを出力する。　　　　　　*
****************************************************************
 IDENTIFICATION         DIVISION.
*
 PROGRAM-ID.            SBM0221B.
*                  流用:SBM0208B ジュンテンドー
 AUTHOR.                NAV.
 DATE-WRITTEN.          2024/06/25.
*
 ENVIRONMENT            DIVISION.
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*支払メッセージ
     SELECT   BMSSHIF   ASSIGN    TO        DA-01-VI-BMSSIHL2
                        ORGANIZATION        INDEXED
                        ACCESS    MODE      SEQUENTIAL
                        RECORD    KEY       SHI-F013  SHI-F308
                        FILE  STATUS   IS   SHI-STS.
*支払ワーク
     SELECT   BMSSH8W   ASSIGN    TO        DA-01-VS-BMSSH8W
                        ORGANIZATION        SEQUENTIAL
                        ACCESS    MODE      SEQUENTIAL
                        FILE  STATUS   IS   SHW-STS.
*
 DATA                   DIVISION.
 FILE                   SECTION.
******************************************************************
*    支払メッセージ　　　　　　　　　
******************************************************************
 FD  BMSSHIF            LABEL RECORD   IS   STANDARD.
     COPY     BMSSIHF   OF        XFDLIB
              JOINING   SHI       PREFIX.
******************************************************************
*    支払ワーク
******************************************************************
 FD  BMSSH8W            LABEL RECORD   IS   STANDARD.
     COPY     BMSSH8W   OF        XFDLIB
              JOINING   SHW       PREFIX.
******************************************************************
 WORKING-STORAGE        SECTION.
******************************************************************
*FLG/ｶｳﾝﾄ
 01  END-FLG                 PIC  X(03)     VALUE  ZERO.
 01  READ-CNT                PIC  9(07)     VALUE  ZERO.
 01  WRITE-CNT               PIC  9(07)     VALUE  ZERO.
*
*システム日付の編集
 01  SYS-WORKAREA.
     03  SYS-DATE          PIC 9(06).
     03  SYS-DATEW         PIC 9(08).
 01  STS-AREA.
     03  SHI-STS           PIC  X(02).
     03  SHW-STS           PIC  X(02).
*
*---<< ｻﾌﾞﾙｰﾁﾝ LINK AREA >>-*
 01  LINK-AREA3.
     03  LI-KBN                  PIC  9.
     03  LI-KETA                 PIC  9.
     03  LI-START                PIC  9(09).
     03  LI-END                  PIC  9(09).
     03  LI-DENNO                PIC  9(09).
     03  L3-ERR                  PIC  9(01).
     03  L3-NEXT                 PIC  9(09).
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SBM0221B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SBM0221B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SBM0221B".
         05  FILLER         PIC   X(11)  VALUE
                                         " ABEND *** ".
     03  ABEND-FILE.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  AB-FILE        PIC   X(08).
         05  FILLER         PIC   X(06)  VALUE " ST = ".
         05  AB-STS         PIC   X(02).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  SEC-NAME.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(07)  VALUE " SEC = ".
         05  S-NAME         PIC   X(30).
*日付サブルーチン用
 01  LINK-AREA.
     03  LINK-IN-KBN        PIC   X(01).
     03  LINK-IN-YMD6       PIC   9(06).
     03  LINK-IN-YMD8       PIC   9(08).
     03  LINK-OUT-RET       PIC   X(01).
     03  LINK-OUT-YMD8      PIC   9(08).
*
 01  WORK-AREA.
     03  WK-CD               PIC  9(04).
     03  WK-CALC             PIC  9(01).
     03  WK-DIV-1            PIC  9(03).
     03  WK-DIV-2            PIC  9(03).
     03  WK-KETA             PIC  9(01).
     03  CALC-CD             PIC  9(01).
     03  WK-DENNO            PIC  9(09).
     03  WK-DENCHK.
         05  WK-DENCHK1      PIC  9(08).
         05  WK-DENCHK2      PIC  9(01).
 LINKAGE                SECTION.
 01  PARA-TOKCD     PIC   9(08).
 01  PARA-SHIME     PIC   9(08).
*
******************************************************************
*             M A I N             M O D U L E                    *
******************************************************************
 PROCEDURE              DIVISION USING  PARA-TOKCD
                                        PARA-SHIME.
*
*
 DECLARATIVES.
 FILEERR-SEC1           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   BMSSHIF.
     MOVE      "BMSSIHL2"   TO   AB-FILE.
     MOVE      SHI-STS      TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 FILEERR-SEC2           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   BMSSH8W.
     MOVE      "BMSSIHW"    TO   AB-FILE.
     MOVE      SHW-STS      TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*
 END     DECLARATIVES.
*****************************************************************
*                                                                *
******************************************************************
 GENERAL-PROCESS       SECTION.
*
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC   UNTIL  END-FLG = "END".
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
     MOVE     "INIT-SEC"          TO   S-NAME.
*
     OPEN     INPUT     BMSSHIF
              OUTPUT    BMSSH8W.
*
     DISPLAY  MSG-START UPON CONS.
*
******************
*システム日付編集*
******************
     ACCEPT      SYS-DATE  FROM      DATE.
     MOVE       "3"        TO        LINK-IN-KBN.
     MOVE        SYS-DATE  TO        LINK-IN-YMD6.
     CALL       "SKYDTCKB"   USING   LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD8.
     IF          LINK-OUT-RET   =    ZERO
         MOVE    LINK-OUT-YMD8  TO   SYS-DATEW
     ELSE
         MOVE    ZERO           TO   SYS-DATEW
     END-IF.
*ファイルスタート
     PERFORM  INSTART-SEC.
     IF   END-FLG = "END"
          DISPLAY NC"＃＃　支払メッセージ無　＃＃"  UPON CONS
          GO                    TO   INIT-EXIT
     END-IF.
*ファイル読込
     PERFORM INREAD-SEC.
*
 INIT-EXIT.
     EXIT.
*
****************************************************************
*    支払メッセージスタート
****************************************************************
 INSTART-SEC                SECTION.
*
     MOVE    "INSTART-SEC"        TO   S-NAME.
*
     MOVE     SPACE               TO   SHI-REC.
     INITIALIZE                        SHI-REC.
*
     MOVE     PARA-TOKCD          TO   SHI-F013.
     MOVE     PARA-SHIME          TO   SHI-F308.
*
     START  BMSSHIF  KEY  IS  >=  SHI-F013 SHI-F308
*
            INVALID
            MOVE    "END"         TO   END-FLG
     END-START.
*
 INSTART-EXIT.
     EXIT.
*
****************************************************************
*   支払メッセージ読込
****************************************************************
 INREAD-SEC                 SECTION.
*
     MOVE    "INREAD-SEC"   TO   S-NAME.
*
     READ     BMSSHIF  NEXT
         AT  END
              MOVE     "END"      TO   END-FLG
              GO                  TO   INREAD-EXIT
     END-READ.
     ADD     1    TO   READ-CNT.
*
 INREAD-EXIT.
     EXIT.
*
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"          TO   S-NAME.
**  抽出条件判定
     IF   SHI-F013  >  PARA-TOKCD
          MOVE   "END"   TO   END-FLG
              GO  TO   MAIN-EXIT
     END-IF.
*対象期間終了
     IF  SHI-F308  >  PARA-SHIME
          MOVE   "END"   TO   END-FLG
              GO  TO   MAIN-EXIT
     END-IF.
*
*支払ワ－ク出力
  MAIN-010.
     MOVE    SHI-REC        TO  SHW-REC.
*
     IF   SHW-F434  NOT  NUMERIC
          MOVE      ZERO    TO  SHW-F434
     END-IF.
*
* 計上部署判定
*   店以外の明細時の値
     IF  SHW-F405(1:3)  =  "0  " OR
         SHW-F405(1:3)  =  "000"
             MOVE   "999"   TO  SHW-F405
     END-IF.
*
* INPUTデータ数値非セット項目
     MOVE    ZERO           TO  SHW-F123
*                               SHW-F307
                                SHW-F421
                                SHW-F423.
*
* 支払内容（個別名称）
*   PENDING:半角混在の可能性あり
     MOVE     SPACE         TO  SHW-F430.
*----MOVE     SHI-F430      TO  SHW-F430.
     EVALUATE SHI-F429
       WHEN "1000" MOVE NC"オンライン基本料"         TO SHW-F430
       WHEN "1001" MOVE NC"オンライン処理料"         TO SHW-F430
       WHEN "1002" MOVE NC"ＥＯＳ伝票代　　"         TO SHW-F430
       WHEN "1003" MOVE NC"ＤＭＭ使用料　　"         TO SHW-F430
       WHEN "1004" MOVE NC"ＰＤラベル発行料"         TO SHW-F430
       WHEN "1005" MOVE NC"ＰＤラベル送料　"         TO SHW-F430
       WHEN "1006" MOVE NC"手書納品書代　　"         TO SHW-F430
       WHEN "1007" MOVE NC"手形郵送料　　　"         TO SHW-F430
       WHEN "1008" MOVE NC"物流センターフィー"       TO SHW-F430
       WHEN "1009" MOVE NC"リベート１（仕入値引）"   TO SHW-F430
       WHEN "1010" MOVE NC"リベート２（仕入割戻）"   TO SHW-F430
       WHEN "1011" MOVE NC"リベート３　　　　　　"   TO SHW-F430
       WHEN "1012" MOVE NC"納品率手数料　　　　　"   TO SHW-F430
       WHEN "1013" MOVE NC"ポイントキャンペーン費用" TO SHW-F430
       WHEN "1014" MOVE NC"食品商品券　　　　　　　" TO SHW-F430
       WHEN "1015" MOVE NC"コンテナ荷卸料　　　　　" TO SHW-F430
       WHEN "1016" MOVE NC"その他１　　　　　　　　" TO SHW-F430
       WHEN "1017" MOVE NC"その他２　　　　　　　　" TO SHW-F430
       WHEN "1021" MOVE NC"８％仕入／返品合計額"     TO SHW-F430
       WHEN "1022" MOVE NC"８％仕入／返品消費税額"   TO SHW-F430
       WHEN "1023" MOVE NC"１０％仕入／返品合計額"   TO SHW-F430
       WHEN "1024" MOVE NC"１０％仕入／返品消費税額" TO SHW-F430
       WHEN "1025" MOVE NC"１０％相殺合計額　　　"   TO SHW-F430
       WHEN "1026" MOVE NC"１０％相殺消費税額　　"   TO SHW-F430
       WHEN "1027" MOVE NC"相殺前支払額　　　　　"   TO SHW-F430
       WHEN "1028" MOVE NC"相殺前消費税額　　　　"   TO SHW-F430
       WHEN "1029" MOVE NC"支払額　　　　　　　　"   TO SHW-F430
       WHEN "1030" MOVE NC"支払額消費税　　　　　"   TO SHW-F430
       WHEN "0001" MOVE NC"株式会社サカタのタネ　"   TO SHW-F430
       WHEN "0002" MOVE NC"株式会社ＨＩヒロセ　　"   TO SHW-F430
       WHEN "    " MOVE ALL NC"　"                   TO SHW-F430
       WHEN "0000" MOVE ALL NC"　"                   TO SHW-F430
       WHEN  OTHER MOVE ALL NC"＊"                   TO SHW-F430
     END-EVALUATE.
*
     WRITE   SHW-REC.
     ADD     1      TO      WRITE-CNT.
 MAIN-200.
     PERFORM  INREAD-SEC.
*
 MAIN-EXIT.
     EXIT.
*
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*件数出力
*支払メッセード読込
     DISPLAY "BMSSIHF   READ CNT = " READ-CNT UPON CONS.
*支払ワ－ク出力
     DISPLAY "BMSSIHW  WRITE CNT = " WRITE-CNT UPON CONS.
*
     CLOSE     BMSSHIF  BMSSH8W.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
