****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ　　　　　　　　*
*    業務名　　　　　　　：　ＥＤＩＣ　　　　　　　　　　　　　*
*    モジュール名　　　　：　受領ＭＳＧ抽出（共通）　　　　　　*
*    作成日／更新日　　　：　2015/08/20                        *
*    作成者／更新者　　　：　ＮＡＶ宮下　　　　　　　　　　　　*
*    ＩＮＰＵＴ　　　　　：　取引先コード PARA-TORICD          *
*                        ：　受信日       PARA-DATE            *
*                        ：　受信時刻     PARA-TIME            *
*    処理概要　　　　　　：　ＩＮＰＵＴパラメタより、ＥＤＩＣ  *
*                        ：　受領ＭＳＧを抽出する　　　　　　  *
*    更新日／更新者　　　：　                                  *
*    更新概要　　　　　　：　　　　　　　　　　　　　　　　　　*
*                                                              *
****************************************************************
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SED0010W.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2015/08/20.
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
*----<< EDIC受領MSG >>--*
     SELECT   EDJYURF   ASSIGN         DA-01-VI-EDJYURL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  JUR-F011
                                       JUR-F012
                                       JUR-F013
                                       JUR-F02
                                       JUR-F03
                                       JUR-F04
                                       JUR-F05
                        STATUS         EDJYURF-ST.
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
*----<< EDIC受領ワーク >>--*
     SELECT   EWJYURF   ASSIGN    TO        DA-01-VS-EWJYURF
                        ACCESS    MODE      IS   SEQUENTIAL
                        FILE      STATUS    IS   JWK-STATUS.
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< EDIC受領MSG >>--*
 FD  EDJYURF            LABEL     RECORD   IS   STANDARD.
     COPY     EDJYURF   OF        XFDLIB
              JOINING   JUR       PREFIX.
*----<< 取引先マスタ >>--*
 FD  HTOKMS             LABEL RECORD   IS   STANDARD.
     COPY     HTOKMS    OF        XFDLIB
              JOINING   TOK       PREFIX.
*----<< 店舗マスタ >>--*
 FD  HTENMS             LABEL RECORD   IS   STANDARD.
     COPY     HTENMS    OF        XFDLIB
              JOINING   TEN       PREFIX.
*----<< EDIC受領ワーク >>--*
 FD  EWJYURF            LABEL     RECORD   IS   STANDARD.
     COPY     EWJYURF   OF        XFDLIB
              JOINING   JWK       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  EDJYURF-ST        PIC  X(02).
 01  HTOKMS-ST         PIC  X(02).
 01  HTENMS-ST         PIC  X(02).
 01  JWK-STATUS        PIC  X(02).
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
*----<< 行番号変換 >>--*
 01  GYONO-SUCHI            PIC   9(04).
 01  FILLER            REDEFINES      GYONO-SUCHI.
     03  GYONO-MOJI         PIC   X(04).
*
*----<< 店舗コード変換 >>--*
 01  TEMPOCD-MOJI           PIC   X(05).
 01  FILLER            REDEFINES      TEMPOCD-MOJI.
     03  TEMPOCD-SUCHI      PIC  9(05).
*
*----<< 数量変換 >>--*
 01  SURYO-MOJI             PIC   X(07).
 01  FILLER            REDEFINES      SURYO-MOJI.
     03  SURYO-SUCHI        PIC   9(06)V9(01).
*
*----<< 単価変換 >>--*
 01  TANKA-MOJI             PIC   X(10).
 01  FILLER            REDEFINES      TANKA-MOJI.
     03  TANKA-SUCHI        PIC   9(08)V9(02).
*
*----<< 金額変換 >>--*
 01  KINGAKU-MOJI           PIC   X(10).
 01  FILLER            REDEFINES      KINGAKU-MOJI.
     03  KINGAKU-SUCHI      PIC   9(10).
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
 01  ZEI-KUBUN              PIC   X(06)  VALUE "ED201".
 01  TEISEI-KUBUN           PIC   X(06)  VALUE "ED188".
*
*----<< ﾍﾝｽｳ >>--*
 01  WK-AREA.
     03  WK-SHORISHUBETSU   PIC   N(20)  VALUE SPACE.
     03  WK-SHORISHUBETSU-K PIC   X(20)  VALUE SPACE.
     03  WK-ZEIKUBUN        PIC   N(20)  VALUE SPACE.
     03  WK-ZEIKUBUN-K      PIC   X(20)  VALUE SPACE.
     03  WK-TEISEIKUBUN     PIC   N(20)  VALUE SPACE.
     03  WK-TEISEIKUBUN-K   PIC   X(20)  VALUE SPACE.
     03  WK-MEISHO-KEKKA1   PIC   X(01)  VALUE SPACE.
     03  WK-MEISHO-KEKKA2   PIC   X(01)  VALUE SPACE.
     03  WK-MEISHO-KEKKA3   PIC   X(01)  VALUE SPACE.
     03  WK-TENPO-CD        PIC   9(13)  VALUE ZERO.

*----<< ﾒｯｾｰｼﾞ ｴﾘｱ >>--*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SED0010W".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SED0010W".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SED0010W".
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
 COPY     KEIJYRF   OF        XFDLIB.
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
*----<< ＥＤＩＣ受領ＭＳＧ >>--*
 EDJYURF-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      EDJYURF.
     MOVE      "EDJYURF"    TO   AB-FILE.
     MOVE      EDJYURF-ST   TO   AB-STS.
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
*----<< ＥＤＩＣ受領ワーク >>--*
 EWJYURF-ERR            SECTION.
     USE AFTER     EXCEPTION PROCEDURE      EWJYURF.
     MOVE      "EWJYURF"    TO   AB-FILE.
     MOVE      JWK-STATUS   TO   AB-STS.
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
     OPEN     INPUT     EDJYURF.
     OPEN     INPUT     HTOKMS.
     OPEN     INPUT     HTENMS.
     OPEN     OUTPUT    EWJYURF.
*
*----<< ﾍﾝｽｳ ｸﾘｱ >--*
     MOVE     ZERO      TO        END-FG    RD-CNT    WRT-CNT.
     MOVE     ZERO      TO        IN-CNT    OUT-CNT.
*
*----<< ﾄﾘﾋｷｻｷMﾖﾐｺﾐ >>--*
     MOVE     PARA-TORICD     TO   TOK-F01.
     PERFORM  900-TOK-READ.
*
*----<< ｷｰｼﾞｮｳﾎｳ ｾｯﾄ >>--*
     MOVE     PARA-TORICD     TO   JUR-F011.
     MOVE     PARA-DATE       TO   JUR-F012.
     MOVE     PARA-TIME       TO   JUR-F013.
*
*----<< FILE START >>--*
     START    EDJYURF  KEY  >=   JUR-F011
                                 JUR-F012
                                 JUR-F013
                                 JUR-F02
                                 JUR-F03
                                 JUR-F04
                                 JUR-F05
        INVALID   KEY
           MOVE      9    TO   END-FG
             GO   TO   INIT-EXIT
     END-START.
*
*----<< 1ｹﾝﾒ ﾖﾐｺﾐ >>--*
     PERFORM  900-JUR-READ.

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
     MOVE     JUR-F212(9:5)   TO   TEMPOCD-MOJI.
     MOVE     TEMPOCD-SUCHI   TO   TEN-F011.
     PERFORM  900-TEN-READ.
*
*----<< EDICﾒｲｼｮｳ ｼｭﾄｸ >>--*
     MOVE     SPACE           TO   MEISHO-AREA.
     MOVE     PARA-TORICD     TO   MEISHO-TORICD.
     MOVE     SHORISHUBETSU-KUBUN
                              TO   MEISHO-KUBUN.
     MOVE     JUR-F234        TO   MEISHO-CODE.
     PERFORM  900-MEISHO-SUB.
     MOVE     MEISHO-MEI      TO   WK-SHORISHUBETSU.
     MOVE     MEISHO-KANA     TO   WK-SHORISHUBETSU-K.
     MOVE     MEISHO-KEKKA    TO   WK-MEISHO-KEKKA1.
*
     MOVE     SPACE           TO   MEISHO-AREA.
     MOVE     PARA-TORICD     TO   MEISHO-TORICD.
     MOVE     ZEI-KUBUN       TO   MEISHO-KUBUN.
     MOVE     JUR-F235        TO   MEISHO-CODE.
     PERFORM  900-MEISHO-SUB.
     MOVE     MEISHO-MEI      TO   WK-ZEIKUBUN.
     MOVE     MEISHO-KANA     TO   WK-ZEIKUBUN-K.
     MOVE     MEISHO-KEKKA    TO   WK-MEISHO-KEKKA2.
*
     MOVE     SPACE           TO   MEISHO-AREA.
     MOVE     PARA-TORICD     TO   MEISHO-TORICD.
     MOVE     TEISEI-KUBUN    TO   MEISHO-KUBUN.
     MOVE     JUR-F319        TO   MEISHO-CODE.
     PERFORM  900-MEISHO-SUB.
     MOVE     MEISHO-MEI      TO   WK-TEISEIKUBUN.
     MOVE     MEISHO-KANA     TO   WK-TEISEIKUBUN-K.
     MOVE     MEISHO-KEKKA    TO   WK-MEISHO-KEKKA3.
*
*----<< WK ｻｸｾｲ >>--*
     MOVE     SPACE          TO   JWK-REC.
     INITIALIZE                   JWK-REC.
*
     MOVE     PARA-DATE      TO   JWK-F011.
     MOVE     PARA-TIME      TO   JWK-F012.
     MOVE     PARA-TORICD    TO   JWK-F013.
     MOVE     TOK-F03        TO   JWK-F02.
     MOVE     JUR-F208       TO   JWK-F03.
     MOVE     JUR-F212       TO   JWK-F04.
     MOVE     TEN-F03        TO   JWK-F05.
     MOVE     JUR-F214       TO   JWK-F06.
*****MOVE     JUR-F231       TO   JWK-F07.
     MOVE     JUR-F03        TO   JWK-F07.
     MOVE     JUR-F234       TO   JWK-F08.
     MOVE     WK-SHORISHUBETSU
                             TO   JWK-F09.
     MOVE     JUR-F235       TO   JWK-F10.
     MOVE     WK-ZEIKUBUN    TO   JWK-F11.
     MOVE     JUR-F05        TO   GYONO-SUCHI.
     MOVE     GYONO-MOJI     TO   JWK-F12.
     MOVE     SPACE          TO   JWK-F13.
     MOVE     JUR-F307       TO   JWK-F14.
     MOVE     JUR-F311       TO   JWK-F15.
     MOVE     JUR-F312       TO   JWK-F16.
*原価金額
     MOVE     JUR-F316       TO   KINGAKU-MOJI.
     COMPUTE  JWK-F17        =    KINGAKU-SUCHI / 100.
*原単価
     MOVE     JUR-F315       TO   TANKA-MOJI.
     MOVE     TANKA-SUCHI    TO   JWK-F18.
*売価金額
     MOVE     JUR-F318       TO   KINGAKU-MOJI.
     COMPUTE  JWK-F19        =    KINGAKU-SUCHI / 100.
*売単価
     MOVE     JUR-F317       TO   KINGAKU-MOJI.
     COMPUTE  JWK-F20        =    KINGAKU-SUCHI / 100.
*発注数量（バラ）
     MOVE     JUR-F319       TO   SURYO-MOJI.
     MOVE     SURYO-SUCHI    TO   JWK-F21.
*出荷数量（バラ）
     MOVE     JUR-F323       TO   SURYO-MOJI.
     MOVE     SURYO-SUCHI    TO   JWK-F22.
*受領数量（バラ）
     MOVE     JUR-F325       TO   SURYO-MOJI.
     MOVE     SURYO-SUCHI    TO   JWK-F23.
     MOVE     JUR-F327       TO   JWK-F24.
     MOVE     WK-TEISEIKUBUN TO   JWK-F25.
*
*----<< WK ｶｷｺﾐ >>--*
     WRITE    JWK-REC.
     ADD      1              TO   WRT-CNT.
*
*----<< ﾂｷﾞﾚｺｰﾄﾞ ﾖﾐｺﾐ >>--*
     PERFORM  900-JUR-READ.

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
     CLOSE     EDJYURF.
     CLOSE     HTOKMS.
     CLOSE     HTENMS.
     CLOSE     EWJYURF.
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
*    LEVEL ALL    ＥＤＩＣ受領ＭＳＧ　 READ                    *
*--------------------------------------------------------------*
 900-JUR-READ           SECTION.
     MOVE     "900-JUR-READ"      TO   S-NAME.
     READ     EDJYURF
       AT END
           MOVE      9    TO   END-FG
       NOT AT END
           ADD       1    TO   RD-CNT
     END-READ.
*
     IF (JUR-F011 = PARA-DATE AND
         JUR-F012 = PARA-TIME AND
         JUR-F013 = PARA-TORICD )
     THEN
         CONTINUE
     ELSE
           MOVE      9    TO   END-FG
     END-IF.
 900-JUR-READ-EXIT.
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