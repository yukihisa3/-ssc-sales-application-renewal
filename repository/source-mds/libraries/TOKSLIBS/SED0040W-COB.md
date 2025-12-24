# SED0040W

**種別**: COBOL プログラム  
**ライブラリ**: TOKSLIBS  
**ソースファイル**: `source/navs/cobol/programs/TOKSLIBS/SED0040W.COB`

## ソースコード

```cobol
****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ　　　　　　　　*
*    業務名　　　　　　　：　ＥＤＩＣ　　　　　　　　　　　　　*
*    モジュール名　　　　：　不照合リストワーク作成　　　　　　*
*    作成日／更新日　　　：　2015/09/02                        *
*    作成者／更新者　　　：　ＮＡＶ宮下　　　　　　　　　　　　*
*    ＩＮＰＵＴ　　　　　：　取引先コード PARA-TORICD          *
*                        ：　受信日       PARA-DATE            *
*                        ：　受信時刻     PARA-TIME            *
*    処理概要　　　　　　：　ＩＮＰＵＴパラメタより、ＥＤＩＣ  *
*                        ：　支払ＭＳＧを抽出する。　　　　　  *
*                        ：　請求合計Ｆを参照して不照合リスト  *
*                        ：　ワークとマッチングする。　　　　  *
*    更新日／更新者　　　：　                                  *
*    更新概要　　　　　　：　　　　　　　　　　　　　　　　　　*
*                                                              *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SED0040W.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2015/09/02.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FUJITSU.
 OBJECT-COMPUTER.       FUJITSU.
 SPECIAL-NAMES.
     CONSOLE  IS        CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*----<< EDIC支払MSG >>--*
     SELECT   EDSIHAF   ASSIGN         DA-01-VI-EDSIHAL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  SIH-F011
                                       SIH-F012
                                       SIH-F013
                                       SIH-F02
                        STATUS         EDSIHAF-ST.
*----<< 請求合計ファイル >>--*
     SELECT   HSEIGKF   ASSIGN    TO   DA-01-VI-SETGKF
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  SEI-F01
                                       SEI-F03
                                       SEI-F05
                        STATUS         HSEIGKF-ST.
*----<< 取引先マスタ >>--*
     SELECT   HTOKMS    ASSIGN         DA-01-VI-TOKMS2
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TOK-F01
                        STATUS         HTOKMS-ST.
*----<< 店舗マスタ >>--*
     SELECT   HTENMS    ASSIGN         DA-01-VI-TENMS1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  TEN-F52
                                       TEN-F011
                        STATUS         HTENMS-ST.
*----<< EDIC不照合リストワーク >>--*
     SELECT   EWFUSHF   ASSIGN    TO   DA-01-VI-EWFUSHL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  FWK-F01
                                       FWK-F05
                                       FWK-F18
                                       FWK-F04
                        STATUS    IS   FWK-STATUS.
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< EDIC支払MSG >>--*
 FD  EDSIHAF            LABEL RECORD   IS   STANDARD.
     COPY     EDSIHAF   OF        XFDLIB
              JOINING   SIH       PREFIX.
*----<< 請求合計Ｆ >>--*
 FD  HSEIGKF            LABEL RECORD   IS   STANDARD.
     COPY     SETGKFP   OF        XFDLIB
              JOINING   SEI       PREFIX.
*----<< 取引先マスタ >>--*
 FD  HTOKMS             LABEL RECORD   IS   STANDARD.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
*----<< 店舗マスタ >>--*
 FD  HTENMS             LABEL RECORD   IS   STANDARD.
     COPY     HTENMS    OF        XFDLIB
              JOINING   TEN       PREFIX.
*----<< EDIC不照合リストワーク >>--*
 FD  EWFUSHF            LABEL RECORD   IS   STANDARD.
     COPY     EWFUSHF   OF        XFDLIB
              JOINING   FWK       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  EDSIHAF-ST        PIC  X(02).
 01  HSEIGKF-ST        PIC  X(02).
 01  HTOKMS-ST         PIC  X(02).
 01  HTENMS-ST         PIC  X(02).
 01  FWK-STATUS        PIC  X(02).
*
*----<< INVALID FLG >>--*
 01  TOK-INVALID-FLG   PIC  X(01).
 01  TEN-INVALID-FLG   PIC  X(01).
 01  FWK-INVALID-FLG   PIC  X(01).
*
*----<< FLAG >>--*
 01  FLAG.
     03  SIH-END-FLAG       PIC  9(01)     VALUE  ZERO.
     03  SEI-END-FLAG       PIC  9(01)     VALUE  ZERO.
*
*----<< ｶｳﾝﾀｰ >>--*
 01  COUNTER.
     03  SIH-RD-CNT         PIC  9(08)     VALUE  ZERO.
     03  SEI-RD-CNT         PIC  9(08)     VALUE  ZERO.
     03  WRT-CNT            PIC  9(08)     VALUE  ZERO.
*
*----<< 店舗コード変換 >>--*
 01  TEMPOCD-MOJI           PIC   X(13).
 01  FILLER            REDEFINES      TEMPOCD-MOJI.
     03  TEMPOCD-SUCHI      PIC  9(13).
*
*----<< 店舗コード逆変換 >>--*
 01  TEMPOCD-SUCHI2         PIC  9(05).
 01  FILLER            REDEFINES      TEMPOCD-SUCHI2.
     03  TEMPOCD-MOJI2      PIC  X(05).
*
*----<< 伝票番号変換 >>--*
 01  DEMPYONO-MOJI          PIC   X(10).
 01  FILLER            REDEFINES      DEMPYONO-MOJI.
     03  DEMPYONO-SUCHI     PIC   9(10).
*
*----<< 金額変換 >>--*
 01  KINGAKU-MOJI           PIC   X(11).
 01  FILLER            REDEFINES      KINGAKU-MOJI.
     03  KINGAKU-SUCHI      PIC   9(11).
*
*----<< 名称マスタ取得用 >>--*
 01  MEISHO-AREA.
     03  MEISHO-TORICD      PIC   9(08).
     03  MEISHO-KUBUN       PIC   X(06).
     03  MEISHO-CODE        PIC   X(10).
     03  MEISHO-MEI         PIC   N(20).
     03  MEISHO-KANA        PIC   X(20).
     03  MEISHO-KEKKA       PIC   X(01).
*
*----<< ｺﾃｲｺｳﾓｸ >>--*
 01  SHORISHUBETSU-KUBUN    PIC   X(06)  VALUE "ED184".
 01  SEIKYU-KUBUN           PIC   X(06)  VALUE "ED189".
 01  MIBARAIKAIKAKE-KUBUN   PIC   X(06)  VALUE "ED190".
 01  SHOGOKEKKA             PIC   X(06)  VALUE "ED191".
 01  SHIHARAI-NAIYO         PIC   X(06)  VALUE "ED192".
 01  SHIHARAIHOHO-KUBUN     PIC   X(06)  VALUE "ED196".
 01  ZEI-KUBUN              PIC   X(06)  VALUE "ED201".
*
*----<< ﾍﾝｽｳ >>--*
 01  WK-AREA.
     03  WK-SHORISHUBETSU   PIC   N(20)  VALUE SPACE.
     03  WK-SHORISHUBETSU-K PIC   X(20)  VALUE SPACE.
     03  WK-SEIKYU          PIC   N(20)  VALUE SPACE.
     03  WK-SEIKYU-K        PIC   X(20)  VALUE SPACE.
     03  WK-MIBARAIKAIKAKE  PIC   N(20)  VALUE SPACE.
     03  WK-MIBARAIKAIKAKE-K
                            PIC   X(20)  VALUE SPACE.
     03  WK-SHOGOKEKKA      PIC   N(20)  VALUE SPACE.
     03  WK-SHOGOKEKKA-K    PIC   X(20)  VALUE SPACE.
     03  WK-SHIHARAI-NAIYO  PIC   N(20)  VALUE SPACE.
     03  WK-SHIHARAI-NAIYO-K
                            PIC   X(20)  VALUE SPACE.
     03  WK-SHIHARAIHOHO    PIC   N(20)  VALUE SPACE.
     03  WK-SHIHARAIHOHO-K  PIC   X(20)  VALUE SPACE.
     03  WK-ZEIKUBUN        PIC   N(20)  VALUE SPACE.
     03  WK-ZEIKUBUN-K      PIC   X(20)  VALUE SPACE.
     03  WK-HEMPIN-NEBIKI   PIC   N(20)  VALUE SPACE.
     03  WK-HEMPIN-NEBIKI-K PIC   X(20)  VALUE SPACE.
     03  WK-MEISHO-KEKKA1   PIC   X(01)  VALUE SPACE.
     03  WK-MEISHO-KEKKA2   PIC   X(01)  VALUE SPACE.
     03  WK-MEISHO-KEKKA3   PIC   X(01)  VALUE SPACE.
     03  WK-MEISHO-KEKKA4   PIC   X(01)  VALUE SPACE.
     03  WK-MEISHO-KEKKA5   PIC   X(01)  VALUE SPACE.
     03  WK-MEISHO-KEKKA6   PIC   X(01)  VALUE SPACE.
     03  WK-MEISHO-KEKKA7   PIC   X(01)  VALUE SPACE.
     03  WK-TENPO-CD        PIC   9(13)  VALUE ZERO.
     03  WK-SHOGO-KEKKA     PIC   X(01)  VALUE SPACE.
     03  WK-SHOGO-NAIYO     PIC   N(10)  VALUE SPACE.
*
*----<< 日付変換サブルーチン用 >>--*
 01  HIDUKE-HENKAN.
     03  LINK-IN-KBN        PIC  X(01).
     03  LINK-IN-YMD6       PIC  9(06).
     03  LINK-IN-YMD8       PIC  9(08).
     03  LINK-OUT-RET       PIC  X(01).
     03  LINK-OUT-YMD       PIC  9(08).
*
*----<< ﾒｯｾｰｼﾞ ｴﾘｱ >>--*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SED0040W".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SED0040W".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SED0040W".
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
     03  MSG-IN.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " INPUT = ".
         05  IN-CNT         PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
     03  MSG-OUT.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  FILLER         PIC   X(09)  VALUE " OUTPUT= ".
         05  OUT-CNT        PIC   9(06).
         05  FILLER         PIC   X(05)  VALUE " *** ".
*
 LINKAGE                SECTION.
 01  PARA-TORICD            PIC   9(08).
 01  PARA-DATE              PIC   9(08).
 01  PARA-TIME              PIC   9(04).
****************************************************************
 PROCEDURE              DIVISION USING  PARA-TORICD
                                        PARA-DATE
                                        PARA-TIME.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< ＥＤＩＣ支払ＭＳＧ >>--*
 EDSIHAF-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      EDSIHAF.
     MOVE      "EDSIHAF"    TO   AB-FILE.
     MOVE      EDSIHAF-ST   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*----<< 請求合計ファイル >>--*
 HSEIGKF-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HSEIGKF.
     MOVE      "HSEIGKF"    TO   AB-FILE.
     MOVE      HSEIGKF-ST   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*----<< 取引先マスタ >>--*
 HTOKMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTOKMS.
     MOVE      "HTOKMS"     TO   AB-FILE.
     MOVE      HTOKMS-ST    TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*----<< 店舗マスタ >>--*
 HTENMS-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HTENMS.
     MOVE      "HTENMS"     TO   AB-FILE.
     MOVE      HTENMS-ST    TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*----<< EDIC不照合リストワーク >>--*
 EWFUSHF-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      EWFUSHF.
     MOVE      "EWFUSHF"    TO   AB-FILE.
     MOVE      FWK-STATUS   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.

 END     DECLARATIVES.
****************************************************************
*                                                              *
****************************************************************
 GENERAL-PROCESS       SECTION.
*
     MOVE     "PROCESS-START"     TO   S-NAME.
     PERFORM  INIT-SEC.
     PERFORM  MAIN-SEC
              UNTIL  SIH-END-FLAG =    9.
     PERFORM  INIT2-SEC.
     PERFORM  MAIN2-SEC
              UNTIL  SEI-END-FLAG =    9.
     PERFORM  END-SEC.
*
****************************************************************
*　　　　　　　初期処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT-SEC               SECTION.
*
*----<< ｼｮｷﾁ ｾｯﾄ >--*
     MOVE     "INIT-SEC"          TO   S-NAME.
*
*----<< ｼｮｷﾒｯｾｰｼﾞ ｼｭﾂﾘｮｸ >>--*
     DISPLAY  MSG-START UPON CONS.
*
*----<<FILE OPEN >>--*
     OPEN     INPUT     EDSIHAF.
     OPEN     INPUT     HSEIGKF.
     OPEN     INPUT     HTOKMS.
     OPEN     INPUT     HTENMS.
     OPEN     I-O       EWFUSHF.
*
*----<< ﾍﾝｽｳ ｸﾘｱ >--*
     MOVE     ZERO      TO        FLAG.
     MOVE     ZERO      TO        COUNTER.
     MOVE     ZERO      TO        IN-CNT    OUT-CNT.
*
*----<< ﾄﾘﾋｷｻｷMﾖﾐｺﾐ >>--*
     MOVE     PARA-TORICD     TO   TOK-F01.
     PERFORM  900-TOK-READ.
*
*----<< ｷｰｼﾞｮｳﾎｳ ｾｯﾄ >>--*
     MOVE     PARA-DATE       TO   SIH-F011.
     MOVE     PARA-TIME       TO   SIH-F012.
     MOVE     PARA-TORICD     TO   SIH-F013.
     MOVE     ZERO            TO   SIH-F02.
*
*----<< FILE START >>--*
     START    EDSIHAF  KEY  >=   SIH-F011
                                 SIH-F012
                                 SIH-F013
                                 SIH-F02
        INVALID   KEY
           MOVE      9    TO   SIH-END-FLAG
             GO   TO   INIT-EXIT
     END-START.
*
*----<< 1ｹﾝﾒ ﾖﾐｺﾐ >>--*
     PERFORM  900-SIH-READ.

 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*
*----<< ﾍﾝｽｳ ｸﾘｱ >>--*
     INITIALIZE                   WK-AREA.
*
*----<< ﾃﾝﾎﾟM ﾖﾐｺﾐ >>--*
     MOVE     PARA-TORICD     TO   TEN-F52.
     MOVE     SIH-F213        TO   TEMPOCD-MOJI.
     MOVE     TEMPOCD-SUCHI   TO   TEMPOCD-SUCHI2.
     MOVE     TEMPOCD-SUCHI2  TO   TEN-F011.
     PERFORM  900-TEN-READ.
*
*----<< EDICﾒｲｼｮｳ ｼｭﾄｸ >>--*
     MOVE     SPACE           TO   MEISHO-AREA.
     MOVE     PARA-TORICD     TO   MEISHO-TORICD.
     MOVE     SHORISHUBETSU-KUBUN
                              TO   MEISHO-KUBUN.
     MOVE     SIH-F225        TO   MEISHO-CODE.
     PERFORM  900-MEISHO-SUB.
     MOVE     MEISHO-MEI      TO   WK-SHORISHUBETSU.
     MOVE     MEISHO-KANA     TO   WK-SHORISHUBETSU-K.
     MOVE     MEISHO-KEKKA    TO   WK-MEISHO-KEKKA1.
*
     MOVE     SPACE           TO   MEISHO-AREA.
     MOVE     PARA-TORICD     TO   MEISHO-TORICD.
     MOVE     SEIKYU-KUBUN    TO   MEISHO-KUBUN.
     MOVE     SIH-F226        TO   MEISHO-CODE.
     PERFORM  900-MEISHO-SUB.
     MOVE     MEISHO-MEI      TO   WK-SEIKYU.
     MOVE     MEISHO-KANA     TO   WK-SEIKYU-K.
     MOVE     MEISHO-KEKKA    TO   WK-MEISHO-KEKKA2.
*
     MOVE     SPACE           TO   MEISHO-AREA.
     MOVE     PARA-TORICD     TO   MEISHO-TORICD.
     MOVE     MIBARAIKAIKAKE-KUBUN
                              TO   MEISHO-KUBUN.
     MOVE     SIH-F227        TO   MEISHO-CODE.
     PERFORM  900-MEISHO-SUB.
     MOVE     MEISHO-MEI      TO   WK-MIBARAIKAIKAKE.
     MOVE     MEISHO-KANA     TO   WK-MIBARAIKAIKAKE-K.
     MOVE     MEISHO-KEKKA    TO   WK-MEISHO-KEKKA3.
*
     MOVE     SPACE           TO   MEISHO-AREA.
     MOVE     PARA-TORICD     TO   MEISHO-TORICD.
     MOVE     SHOGOKEKKA      TO   MEISHO-KUBUN.
     MOVE     SIH-F228        TO   MEISHO-CODE.
     PERFORM  900-MEISHO-SUB.
     MOVE     MEISHO-MEI      TO   WK-SHOGOKEKKA.
     MOVE     MEISHO-KANA     TO   WK-SHOGOKEKKA-K.
     MOVE     MEISHO-KEKKA    TO   WK-MEISHO-KEKKA4.
*
     MOVE     SPACE           TO   MEISHO-AREA.
     MOVE     PARA-TORICD     TO   MEISHO-TORICD.
     MOVE     SHIHARAI-NAIYO  TO   MEISHO-KUBUN.
     MOVE     SIH-F229        TO   MEISHO-CODE.
     PERFORM  900-MEISHO-SUB.
     MOVE     MEISHO-MEI      TO   WK-SHIHARAI-NAIYO.
     MOVE     MEISHO-KANA     TO   WK-SHIHARAI-NAIYO-K.
     MOVE     MEISHO-KEKKA    TO   WK-MEISHO-KEKKA5.
*
     MOVE     SPACE           TO   MEISHO-AREA.
     MOVE     PARA-TORICD     TO   MEISHO-TORICD.
     MOVE     SHIHARAIHOHO-KUBUN
                              TO   MEISHO-KUBUN.
     MOVE     SIH-F233        TO   MEISHO-CODE.
     PERFORM  900-MEISHO-SUB.
     MOVE     MEISHO-MEI      TO   WK-SHIHARAIHOHO.
     MOVE     MEISHO-KANA     TO   WK-SHIHARAIHOHO-K.
     MOVE     MEISHO-KEKKA    TO   WK-MEISHO-KEKKA6.
*
     MOVE     SPACE           TO   MEISHO-AREA.
     MOVE     PARA-TORICD     TO   MEISHO-TORICD.
     MOVE     ZEI-KUBUN       TO   MEISHO-KUBUN.
     MOVE     SIH-F234        TO   MEISHO-CODE.
     PERFORM  900-MEISHO-SUB.
     MOVE     MEISHO-MEI      TO   WK-ZEIKUBUN.
     MOVE     MEISHO-KANA     TO   WK-ZEIKUBUN-K.
     MOVE     MEISHO-KEKKA    TO   WK-MEISHO-KEKKA7.
*
*----<< WK ｻｸｾｲ >>--*
     MOVE     SPACE          TO   FWK-REC.
     INITIALIZE                   FWK-REC.
*
     MOVE     PARA-TORICD    TO   FWK-F01.
     MOVE     TOK-F03        TO   FWK-F02.
     MOVE     SIH-F210       TO   FWK-F03.
     MOVE     SIH-F212(2:9)  TO   FWK-F04.
     MOVE     TEMPOCD-MOJI2  TO   FWK-F05.
     MOVE     TEN-F03        TO   FWK-F06.
     MOVE     SPACE          TO   FWK-F07.
     MOVE     SIH-F218       TO   FWK-F08.
     MOVE     SIH-F225       TO   FWK-F09.
     MOVE     WK-SHORISHUBETSU
                             TO   FWK-F10.
     MOVE     SIH-F226       TO   FWK-F11.
     MOVE     WK-SEIKYU      TO   FWK-F12.
     MOVE     SIH-F227       TO   FWK-F13.
     MOVE     WK-MIBARAIKAIKAKE
                             TO   FWK-F14.
     MOVE     SIH-F228       TO   FWK-F15.
     MOVE     WK-SHOGOKEKKA  TO   FWK-F16.
     MOVE     SPACE          TO   FWK-F17.
     MOVE     SIH-F229       TO   FWK-F18.
     MOVE     WK-SHIHARAI-NAIYO
                             TO   FWK-F19.
     MOVE     SIH-F232       TO   FWK-F20.
     MOVE     SIH-F233       TO   FWK-F21.
     MOVE     WK-SHIHARAIHOHO
                             TO   FWK-F22.
     MOVE     SIH-F234       TO   FWK-F23.
     MOVE     WK-ZEIKUBUN    TO   FWK-F24.
     MOVE     SIH-F221       TO   FWK-F25.
     MOVE     SIH-F222       TO   FWK-F26.
*請求金額
     MOVE     SIH-F223       TO   KINGAKU-MOJI.
     COMPUTE  FWK-F27        =    KINGAKU-SUCHI / 100.
     IF  FWK-F25  =  "-"
         COMPUTE  FWK-F27    =    FWK-F27 * -1
     END-IF.
*支払金額
     MOVE     SIH-F224       TO   KINGAKU-MOJI.
     COMPUTE  FWK-F28        =    KINGAKU-SUCHI / 100.
     IF  FWK-F26  =  "-"
         COMPUTE  FWK-F28    =    FWK-F28 * -1
     END-IF.
     MOVE     "2"            TO   FWK-F32.
     MOVE     NC"請求データ無"
                             TO   FWK-F33.
*
*----<< ワーク書込 >>--*
     MOVE     SIH-F212       TO   DEMPYONO-MOJI.
     IF  DEMPYONO-SUCHI  NOT =  ZERO
         WRITE    FWK-REC
         ADD      1              TO   WRT-CNT
     END-IF.
*
*----<< ﾂｷﾞﾚｺｰﾄﾞ ﾖﾐｺﾐ >>--*
     PERFORM  900-SIH-READ.

 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　初期処理２　　　　　　　　　　　　　　　　　　　*
****************************************************************
 INIT2-SEC              SECTION.
*
*----<< ｷｰｼﾞｮｳﾎｳ ｾｯﾄ >>--*
     INITIALIZE                  SEI-REC.
*
*----<< FILE START >>--*
     START    HSEIGKF  KEY  >=   SEI-F01
                                 SEI-F03
                                 SEI-F05
        INVALID   KEY
           MOVE      9    TO   SEI-END-FLAG
             GO   TO   INIT2-EXIT
     END-START.
*
*----<< 1ｹﾝﾒ ﾖﾐｺﾐ >>--*
     PERFORM  900-SEI-READ.

 INIT2-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理２　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN2-SEC     SECTION.
*
     MOVE    "MAIN2-SEC"          TO   S-NAME.
*
*----<< 変数クリア >>--*
     MOVE     SPACE           TO   WK-SHOGO-KEKKA.
*
*----<< 不照合リストワーク読み込み >>--*
*取引先コード
     MOVE     SEI-F01         TO   FWK-F01.
*店舗コード
     MOVE     SEI-F03         TO   FWK-F05.
*伝票区分
     EVALUATE SEI-F07
         WHEN "40"
              MOVE  "1001"    TO   FWK-F18
         WHEN "41"
              MOVE  "1002"    TO   FWK-F18
         WHEN OTHER
              MOVE  "1001"    TO   FWK-F18
     END-EVALUATE.
*伝票番号
     MOVE     SEI-F05         TO   FWK-F04.
     PERFORM  900-WK-READ.
*
*----<< チェック >>--*
* 存在チェック
     IF  FWK-INVALID-FLG = SPACE
* 金額チェック
         IF  SEI-F06 = FWK-F28
             MOVE  "1"        TO   WK-SHOGO-KEKKA
             MOVE  NC"照合"   TO   WK-SHOGO-NAIYO
         ELSE
             MOVE  "4"        TO   WK-SHOGO-KEKKA
             MOVE  NC"金額違い"
                              TO   WK-SHOGO-NAIYO
         END-IF
         PERFORM  WK-UPDATE-SEC
     ELSE
         MOVE  "3"            TO   WK-SHOGO-KEKKA
         MOVE  NC"支払データ無"
                              TO   WK-SHOGO-NAIYO
         PERFORM  WK-WRITE-SEC
     END-IF.
*
*----<< 次レコード読み込み >>--*
     PERFORM  900-SEI-READ.
*
 MAIN2-EXIT.
     EXIT.
****************************************************************
*　　　　　　　ワーク登録　　　　　　　　　　　　　　　　　　　*
****************************************************************
 WK-WRITE-SEC  SECTION.
*
*----<< 初期化 >>--*
     MOVE     SPACE          TO   FWK-REC.
     INITIALIZE                   FWK-REC.
*
*----<< 店舗Ｍ読み込み >>--*
     MOVE     PARA-TORICD     TO   TEN-F52.
     MOVE     SEI-F03         TO   TEN-F011.
     PERFORM  900-TEN-READ.
*
*----<< 項目セット>>--*
     MOVE     PARA-TORICD    TO   FWK-F01.
     MOVE     TOK-F03        TO   FWK-F02.
     INITIALIZE                   HIDUKE-HENKAN.
     MOVE     "3"            TO   LINK-IN-KBN.
     MOVE     SEI-F02        TO   LINK-IN-YMD6.
     MOVE     ZERO           TO   LINK-IN-YMD8.
     CALL     "SKYDTCKB"     USING   LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD.
     MOVE     LINK-OUT-YMD   TO   FWK-F03.
     MOVE     SEI-F05        TO   FWK-F04.
     MOVE     SEI-F03        TO   FWK-F05.
     MOVE     TEN-F03        TO   FWK-F06.
     MOVE     TEN-F04        TO   FWK-F07.
     INITIALIZE                   HIDUKE-HENKAN.
     MOVE     "3"            TO   LINK-IN-KBN.
     MOVE     SEI-F03        TO   LINK-IN-YMD6.
     MOVE     ZERO           TO   LINK-IN-YMD8.
     CALL     "SKYDTCKB"     USING   LINK-IN-KBN
                                     LINK-IN-YMD6
                                     LINK-IN-YMD8
                                     LINK-OUT-RET
                                     LINK-OUT-YMD.
     MOVE     LINK-OUT-YMD   TO   FWK-F08.
     EVALUATE SEI-F07
         WHEN "40"
              MOVE  "01"     TO   FWK-F09
              MOVE  NC"仕入" TO   FWK-F10
              MOVE  "1001"   TO   FWK-F18
              MOVE  NC"仕入明細"
                             TO   FWK-F19
         WHEN "41"
              MOVE  "02"     TO   FWK-F09
              MOVE  NC"返品" TO   FWK-F10
              MOVE  "1002"   TO   FWK-F18
              MOVE  NC"返品明細"
                             TO   FWK-F19
         WHEN "42"
              MOVE  "03"     TO   FWK-F09
              MOVE  NC"値引" TO   FWK-F10
              MOVE  "1004"   TO   FWK-F18
              MOVE  NC"値引明細"
                             TO   FWK-F19
         WHEN OTHER
              MOVE  SPACE    TO   FWK-F09
              MOVE  ALL NC"＊"
                             TO   FWK-F10
              MOVE  SPACE    TO   FWK-F18
              MOVE  ALL NC"＊"
                             TO   FWK-F19
     END-EVALUATE.
     MOVE     "00"           TO   FWK-F11.
     MOVE     SPACE          TO   FWK-F12.
     MOVE     "01"           TO   FWK-F13.
     MOVE     SPACE          TO   FWK-F14.
     MOVE     "00"           TO   FWK-F15.
     MOVE     SPACE          TO   FWK-F16.
     MOVE     SPACE          TO   FWK-F17.
     MOVE     SPACE          TO   FWK-F20.
     MOVE     SPACE          TO   FWK-F21.
     MOVE     SPACE          TO   FWK-F22.
     MOVE     "00"           TO   FWK-F23.
     MOVE     SPACE          TO   FWK-F24.
     MOVE     SPACE          TO   FWK-F25.
     MOVE     SPACE          TO   FWK-F26.
     MOVE     ZERO           TO   FWK-F27.
     MOVE     ZERO           TO   FWK-F28.
     MOVE     LINK-OUT-YMD   TO   FWK-F29.
     IF       SEI-F06 > 0
              MOVE  "+"      TO   FWK-F30
     ELSE
              MOVE  "-"      TO   FWK-F30
     END-IF.
     MOVE     SEI-F06        TO   FWK-F31.
     MOVE     WK-SHOGO-KEKKA TO   FWK-F32.
     MOVE     WK-SHOGO-NAIYO TO   FWK-F33.
     MOVE     SPACE          TO   FWK-F34.
*
*----<< ワーク書込 >>--*
     WRITE    FWK-REC.
     ADD      1              TO   WRT-CNT.
*
 WK-WRITE-EXIT.
     EXIT.
****************************************************************
*　　　　　　　ワーク更新　　　　　　　　　　　　　　　　　　　*
****************************************************************
 WK-UPDATE-SEC  SECTION.
*
*----<< 項目セット>>--*
     MOVE     SEI-F10        TO   FWK-F29.
     IF       SEI-F06 > 0
              MOVE  "+"      TO   FWK-F30
     ELSE
              MOVE  "-"      TO   FWK-F30
     END-IF.
     MOVE     SEI-F06        TO   FWK-F31.
     MOVE     WK-SHOGO-KEKKA TO   FWK-F32.
     MOVE     WK-SHOGO-NAIYO TO   FWK-F33.
*     MOVE     SPACE          TO   FWK-F34.
*
*----<< ワーク書込 >>--*
     REWRITE  FWK-REC.
*
 WK-UPDATE-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     MOVE      SIH-RD-CNT
                         TO      IN-CNT.
     DISPLAY   MSG-IN    UPON CONS.
     MOVE      SEI-RD-CNT
                         TO      IN-CNT.
     DISPLAY   MSG-IN    UPON CONS.
     MOVE      WRT-CNT   TO      OUT-CNT.
     DISPLAY   MSG-OUT   UPON CONS.
     DISPLAY   MSG-END   UPON CONS.
*
*----<<FILE CLOSE >>--*
     CLOSE     EDSIHAF.
     CLOSE     HSEIGKF.
     CLOSE     HTOKMS.
     CLOSE     HTENMS.
     CLOSE     EWFUSHF.
*
     IF  OUT-CNT = ZERO
         MOVE      4001         TO   PROGRAM-STATUS
     END-IF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    ＥＤＩＣ支払ＭＳＧ　 READ                    *
*--------------------------------------------------------------*
 900-SIH-READ           SECTION.
     MOVE     "900-SIH-READ"      TO   S-NAME.
     READ     EDSIHAF
       AT END
           MOVE      9    TO   SIH-END-FLAG
     END-READ.
*
     IF (SIH-F011 = PARA-DATE AND
         SIH-F012 = PARA-TIME AND
         SIH-F013 = PARA-TORICD )
     THEN
           ADD       1    TO   SIH-RD-CNT
     ELSE
           MOVE      9    TO   SIH-END-FLAG
     END-IF.
 900-SIH-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    請求合計ファイル　 READ                      *
*--------------------------------------------------------------*
 900-SEI-READ           SECTION.
*
*----<< 処理名セット >>--*
     MOVE     "900-SIH-READ"      TO   S-NAME.
*
*----<< 読み込み >>--*
     READ     HSEIGKF
       AT END
           MOVE     9     TO   SEI-END-FLAG
           GO   TO   900-SEI-READ-EXIT
       NOT AT END
           ADD      1     TO   SEI-RD-CNT
     END-READ.
 900-SEI-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    取引先マスタ　 READ                          *
*--------------------------------------------------------------*
 900-TOK-READ           SECTION.
     MOVE     "900-TOK-READ"      TO   S-NAME.
     READ     HTOKMS    INVALID
              MOVE      SPACE          TO   TOK-F03
     END-READ.
 900-TOK-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    店舗マスタ　 READ                            *
*--------------------------------------------------------------*
 900-TEN-READ           SECTION.
     MOVE     "900-TEN-READ"      TO   S-NAME.
     READ     HTENMS    INVALID
              MOVE      SPACE          TO   TEN-F03
     END-READ.
 900-TEN-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    ワーク　 READ                                *
*--------------------------------------------------------------*
 900-WK-READ            SECTION.
*
*----<< 処理名セット >>--*
     MOVE     "900-WK-READ"       TO   S-NAME.
*
*----<< 読み込み >>--*
     READ     EWFUSHF
       INVALID
           MOVE      "1"            TO   FWK-INVALID-FLG
       NOT INVALID
           MOVE      SPACE          TO   FWK-INVALID-FLG
     END-READ.
 900-WK-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    名称マスタサブルーチン                       *
*--------------------------------------------------------------*
 900-MEISHO-SUB         SECTION.
     CALL    "SED9001B" USING     MEISHO-TORICD
                                  MEISHO-KUBUN
                                  MEISHO-CODE
                                  MEISHO-MEI
                                  MEISHO-KANA
                                  MEISHO-KEKKA.
 900-MEISHO-SUB-EXIT.
     EXIT.
*-------------< PROGRAM END >------------------------------------*

```
