****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ　　　　　　　　*
*    業務名　　　　　　　：　ＥＤＩＣ　　　　　　　　　　　　　*
*    モジュール名　　　　：　支払ＭＳＧ抽出（共通）　　　　　　*
*    作成日／更新日　　　：　2015/08/25                        *
*    作成者／更新者　　　：　ＮＡＶ宮下　　　　　　　　　　　　*
*    ＩＮＰＵＴ　　　　　：　取引先コード PARA-TORICD          *
*                        ：　受信日       PARA-DATE            *
*                        ：　受信時刻     PARA-TIME            *
*    処理概要　　　　　　：　ＩＮＰＵＴパラメタより、ＥＤＩＣ  *
*                        ：　支払ＭＳＧを抽出する　　　　　　  *
*    更新日／更新者　　　：　2015/11/11 NAV TAKAHASHI          *
*    更新概要　　　　　　：　店舗ＣＤ＝０の時、店舗ＣＤ＝９９９*
*                            をセットする。                    *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SED0030W.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2015/08/25.
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
*----<< EDIC支払明細書ワーク >>--*
     SELECT   EWSIHAF   ASSIGN    TO        DA-01-VS-EWSIHAF
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   SWK-STATUS.
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< EDIC支払MSG >>--*
 FD  EDSIHAF            LABEL     RECORD   IS   STANDARD.
     COPY     EDSIHAF   OF        XFDLIB
              JOINING   SIH       PREFIX.
*----<< 取引先マスタ >>--*
 FD  HTOKMS             LABEL     RECORD   IS   STANDARD.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
*----<< 店舗マスタ >>--*
 FD  HTENMS             LABEL     RECORD   IS   STANDARD.
     COPY     HTENMS    OF        XFDLIB
              JOINING   TEN       PREFIX.
*----<< EDIC支払明細書ワーク >>--*
 FD  EWSIHAF            LABEL     RECORD   IS   STANDARD.
     COPY     EWSIHAF   OF        XFDLIB
              JOINING   SWK       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  EDSIHAF-ST        PIC  X(02).
 01  HTOKMS-ST         PIC  X(02).
 01  HTENMS-ST         PIC  X(02).
 01  SWK-STATUS        PIC  X(02).
*
*----<< INVALID FLG >>--*
 01  TOK-INVALID-FLG   PIC  X(01).
 01  TEN-INVALID-FLG   PIC  X(01).
*
*----<< ｶｳﾝﾄ >>--*
 01  END-FG                  PIC  9(01)     VALUE  ZERO.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT                 PIC  9(08)     VALUE  ZERO.
*
*----<< 店舗コード変換 >>--*
 01  TEMPOCD-MOJI           PIC   X(05).
 01  FILLER            REDEFINES      TEMPOCD-MOJI.
     03  TEMPOCD-SUCHI      PIC  9(05).
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

*----<< ﾒｯｾｰｼﾞ ｴﾘｱ >>--*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SED0030W".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SED0030W".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SED0030W".
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
*----<< EDIC支払明細書ワーク >>--*
 EWSIHAF-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      EWSIHAF.
     MOVE      "EWSIHAF"    TO   AB-FILE.
     MOVE      SWK-STATUS   TO   AB-STS.
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
              UNTIL     END-FG    =    9.
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
     OPEN     INPUT     HTOKMS.
     OPEN     INPUT     HTENMS.
     OPEN     OUTPUT    EWSIHAF.
*
*----<< ﾍﾝｽｳ ｸﾘｱ >--*
     MOVE     ZERO      TO        END-FG    RD-CNT    WRT-CNT.
     MOVE     ZERO      TO        IN-CNT    OUT-CNT.
     MOVE     SPACE     TO        SIH-REC.
     INITIALIZE                   SIH-REC.
*
*----<< ﾄﾘﾋｷｻｷMﾖﾐｺﾐ >>--*
     MOVE     PARA-TORICD     TO   TOK-F01.
     PERFORM  900-TOK-READ.
*
*----<< ｷｰｼﾞｮｳﾎｳ ｾｯﾄ >>--*
     MOVE     PARA-DATE       TO   SIH-F011.
     MOVE     PARA-TIME       TO   SIH-F012.
     MOVE     PARA-TORICD     TO   SIH-F013.
*
*----<< FILE START >>--*
     START    EDSIHAF  KEY  >=   SIH-F011
                                 SIH-F012
                                 SIH-F013
                                 SIH-F02
        INVALID   KEY
           MOVE      9    TO   END-FG
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
     MOVE     SIH-F213(9:5)   TO   TEMPOCD-MOJI.
     MOVE     TEMPOCD-SUCHI   TO   TEN-F011.
     PERFORM  900-TEN-READ.

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
     MOVE     SPACE          TO   SWK-REC.
     INITIALIZE                   SWK-REC.
*
     MOVE     PARA-TORICD    TO   SWK-F01.
     MOVE     TOK-F03        TO   SWK-F02.
     MOVE     SIH-F210       TO   SWK-F03.
     MOVE     SIH-F212       TO   SWK-F04.
     MOVE     SIH-F213       TO   SWK-F05.
     MOVE     TEN-F03        TO   SWK-F06.
*## 2015/11/11 NAV ST
     IF       SIH-F213  =  "0000000000000"
              MOVE "9999999999999"  TO   SWK-F05
              MOVE NC"支払詳細情報" TO   SWK-F06
     END-IF.
*## 2015/11/11 NAV ED
     MOVE     SPACE          TO   SWK-F07.
     MOVE     SIH-F218       TO   SWK-F08.
     MOVE     SIH-F225       TO   SWK-F09.
     MOVE     WK-SHORISHUBETSU
                             TO   SWK-F10.
     MOVE     SIH-F226       TO   SWK-F11.
     MOVE     WK-SEIKYU      TO   SWK-F12.
     MOVE     SIH-F227       TO   SWK-F13.
     MOVE     WK-MIBARAIKAIKAKE
                             TO   SWK-F14.
     MOVE     SIH-F228       TO   SWK-F15.
     MOVE     WK-SHOGOKEKKA  TO   SWK-F16.
*支払対象
     EVALUATE SIH-F228
         WHEN "00"
              MOVE  NC"支払対象"
                             TO   SWK-F17
         WHEN "01"
              MOVE  NC"支払対象"
                             TO   SWK-F17
         WHEN "02"
              MOVE  NC"支払対象"
                             TO   SWK-F17
         WHEN "03"
              MOVE  NC"支払対象外"
                             TO   SWK-F17
         WHEN "04"
              MOVE  NC"支払対象"
                             TO   SWK-F17
         WHEN OTHER
              MOVE  SPACE    TO   SWK-F17
     END-EVALUATE.
     MOVE     SIH-F229       TO   SWK-F18.
     MOVE     WK-SHIHARAI-NAIYO
                             TO   SWK-F19.
     MOVE     SIH-F232       TO   SWK-F20.
     MOVE     SIH-F233       TO   SWK-F21.
     MOVE     WK-SHIHARAIHOHO
                             TO   SWK-F22.
     MOVE     SIH-F234       TO   SWK-F23.
     MOVE     WK-ZEIKUBUN    TO   SWK-F24.
     MOVE     SIH-F221       TO   SWK-F25.
     MOVE     SIH-F222       TO   SWK-F26.
*請求金額
     MOVE     SIH-F223       TO   KINGAKU-MOJI.
     COMPUTE  SWK-F27        =    KINGAKU-SUCHI / 100.
     IF  SWK-F25  =  "-"
         COMPUTE  SWK-F27    =    SWK-F27 * -1
     END-IF.
*支払金額
     MOVE     SIH-F224       TO   KINGAKU-MOJI.
     COMPUTE  SWK-F28        =    KINGAKU-SUCHI / 100.
     IF  SWK-F26  =  "-"
         COMPUTE  SWK-F28    =    SWK-F28 * -1
     END-IF.
*
*----<< WK ｶｷｺﾐ >>--*
     WRITE    SWK-REC.
     ADD      1              TO   WRT-CNT.
*
*----<< ﾂｷﾞﾚｺｰﾄﾞ ﾖﾐｺﾐ >>--*
     PERFORM  900-SIH-READ.

 MAIN-EXIT.
     EXIT.
****************************************************************
*　　　　　　　終了処理　　　　　　　　　　　　　　　　　　　　*
****************************************************************
 END-SEC       SECTION.
*
     MOVE     "END-SEC"  TO      S-NAME.
*
     MOVE      RD-CNT    TO      IN-CNT.
     MOVE      WRT-CNT   TO      OUT-CNT.
     DISPLAY   MSG-IN    UPON CONS.
     DISPLAY   MSG-OUT   UPON CONS.
     DISPLAY   MSG-END   UPON CONS.
*
*----<<FILE CLOSE >>--*
     CLOSE     EDSIHAF.
     CLOSE     HTOKMS.
     CLOSE     HTENMS.
     CLOSE     EWSIHAF.
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
           MOVE      9    TO   END-FG
       NOT AT END
           ADD       1    TO   RD-CNT
     END-READ.
*
     IF (SIH-F011 = PARA-DATE AND
         SIH-F012 = PARA-TIME AND
         SIH-F013 = PARA-TORICD )
     THEN
         CONTINUE
     ELSE
           MOVE      9    TO   END-FG
     END-IF.
 900-SIH-READ-EXIT.
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
