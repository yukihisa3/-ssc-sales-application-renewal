****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ　　　　　　　　*
*    業務名　　　　　　　：　受注自動欠品　　　　　　　　　　　*
*    モジュール名　　　　：　受信自動欠品処理　　　　　　　　　*
*    作成日／更新日　　　：　2017/05/23                        *
*    作成者／更新者　　　：　ＮＡＶ高橋　　　　　　　　　　　　*
*    ＩＮＰＵＴ　　　　　：　取引先コード PARA-TORICD          *
*                        ：　受信日       PARA-DATE            *
*                        ：　受信時刻     PARA-TIME            *
*                        ：　納品日開始   PARA-NOBIS           *
*                        ：　納品日終了   PARA-NOBIE           *
*                        ：　欠品区分     PARA-KBN             *
*    処理概要　　　　　　：　売上伝票Ｆを読み、出荷制限Ｍを参  *
*                        ：　照して出荷数を０にする。　　　　  *
*                        ：　処理結果を自動欠品結果Ｆに更新す  *
*                        ：　る。　　　　　　　　　　　　　　  *
*    更新日／更新者　　　：　2018/03/15 ＮＡＶ高橋
*    更新概要　　　　　　：　受注残ファイル更新追加　　　　　　*
*                                                              *
****************************************************************
*<履歴>*********************************************************
* XXXX/XX/XX XXXXXXXXX ＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮＮ
* 2018/03/15 高橋　　　受注残ファイル更新処理を追加　　　
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SJK0100B.
 AUTHOR.                NAV.
 DATE-WRITTEN.          2017/05/23.
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
*----<< 売上伝票ファイル >>--*
     SELECT   SHTDENF   ASSIGN         DA-01-VI-SHTDENLA
                        ORGANIZATION   INDEXED
                        ACCESS    MODE SEQUENTIAL
                        RECORD    KEY  DEN-F46
                                       DEN-F47
                                       DEN-F01
                                       DEN-F48
                                       DEN-F02
                                       DEN-F04
                                       DEN-F051
                                       DEN-F07
                                       DEN-F112
                                       DEN-F03
                        STATUS         SHTDENF-ST.
*----<< 出荷制限マスタ >>--*
     SELECT   SYUSGNF   ASSIGN         DA-01-VI-SYUSGNL1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  SYU-F01
                                       SYU-F02
                                       SYU-F03
                        STATUS         SYUSGNF-ST.
*----<< 商品名称マスタ >>--*
     SELECT   HMEIMS    ASSIGN         DA-01-VI-MEIMS1
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  MEI-F011
                                       MEI-F0121
                                       MEI-F0122
                                       MEI-F0123
                        STATUS         HMEIMS-ST.
*----<< 自動欠品結果ファイル >>--*
     SELECT   JIDKEKF   ASSIGN         DA-01-VI-JIDKEKLA
                        ORGANIZATION   INDEXED
                        ACCESS    MODE RANDOM
                        RECORD    KEY  JID-F46
                                       JID-F47
                                       JID-F01
                                       JID-F48
                                       JID-F02
                                       JID-F04
                                       JID-F051
                                       JID-F07
                                       JID-F112
                                       JID-F03
                        STATUS         JIDKEKF-ST.
****************************************************************
 DATA                   DIVISION.
****************************************************************
 FILE                   SECTION.
*----<< 売上伝票ファイル >>--*
 FD  SHTDENF            LABEL     RECORD   IS   STANDARD.
     COPY     SHTDENF   OF        XFDLIB
              JOINING   DEN       PREFIX.
*----<< 出荷制限マスタ >>--*
 FD  SYUSGNF            LABEL RECORD   IS   STANDARD.
     COPY     SYUSGNF   OF        XFDLIB
              JOINING   SYU       PREFIX.
*----<< 商品名称マスタ >>--*
 FD  HMEIMS             LABEL RECORD   IS   STANDARD.
     COPY     HMEIMS    OF        XFDLIB
              JOINING   MEI       PREFIX.
*----<< 自動欠品結果ファイル >>--*
 FD  JIDKEKF            LABEL     RECORD   IS   STANDARD.
     COPY     JIDKEKF   OF        XFDLIB
              JOINING   JID       PREFIX.
*--------------------------------------------------------------*
 WORKING-STORAGE        SECTION.
*--------------------------------------------------------------*
*----<< ﾌｱｲﾙ ｽﾃｰﾀｽ >>--*
 01  SHTDENF-ST        PIC  X(02).
 01  SYUSGNF-ST        PIC  X(02).
 01  HMEIMS-ST         PIC  X(02).
 01  JIDKEKF-ST        PIC  X(02).
*
*----<< INVALID FLG >>--*
 01  SYU-INVALID-FLG   PIC  X(01).
 01  JID-INVALID-FLG   PIC  X(01).
 01  MEI-INVALID-FLG   PIC  X(01).
*
*----<< FLAG >>--*
 01  TAISHO-FLG        PIC  X(01).
*
*----<< ｶｳﾝﾄ >>--*
 01  END-FG                  PIC  9(01)     VALUE  ZERO.
 01  RD-CNT                  PIC  9(08)     VALUE  ZERO.
 01  WRT-CNT                 PIC  9(08)     VALUE  ZERO.
*
*----<< 在庫更新サブルーチン用 >>--*
 01  ZAIKO-UPDATE-AREA.
     03  ZAIKO-MODE          PIC  X(01)     VALUE  "1".
     03  ZAIKO-TORICD        PIC  9(08).
     03  ZAIKO-AITESHOHINCD  PIC  X(13).
     03  ZAIKO-SOKOCD        PIC  X(02).
     03  ZAIKO-SHOHINCD      PIC  X(08).
     03  ZAIKO-HINTANCD      PIC  X(08).
     03  ZAIKO-NOHINBI       PIC  9(08).
     03  HENKOMAE-SURYO      PIC  9(09).
     03  HENKOGO-SURYO       PIC  9(09)     VALUE  ZERO.
     03  ZAIKO-TANABAN       PIC  X(06).
     03  ZAIKO-HIKIATE-FLG   PIC  X(01).
*
*----<< ﾒｯｾｰｼﾞ ｴﾘｱ >>--*
 01  MSG-AREA.
     03  MSG-START.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  ST-PG          PIC   X(08)  VALUE "SJK0100B".
         05  FILLER         PIC   X(11)  VALUE
                                         " START *** ".
     03  MSG-END.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SJK0100B".
         05  FILLER         PIC   X(11)  VALUE
                                         " END   *** ".
     03  MSG-ABEND.
         05  FILLER         PIC   X(05)  VALUE " *** ".
         05  END-PG         PIC   X(08)  VALUE "SJK0100B".
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
*#2018/03/15 NAV ST
*受注残ファイル更新用ワーク
 01  PARA-AREA.
     03  PARA-KOSINKBN         PIC X(01).
     03  PARA-DTKBN            PIC X(01).
     03  PARA-TOKCD            PIC 9(08).
     03  PARA-DENNO            PIC 9(09).
     03  PARA-GYO              PIC 9(02).
     03  PARA-SOUSAI           PIC 9(01).
     03  PARA-DENKU            PIC 9(02).
     03  PARA-TENCD            PIC 9(05).
     03  PARA-SOKCD            PIC X(02).
     03  PARA-HATYU            PIC 9(08).
     03  PARA-NOUHIN           PIC 9(08).
     03  PARA-SYUKA            PIC 9(08).
     03  PARA-SAKATACD         PIC X(08).
     03  PARA-HINTAN1          PIC X(05).
     03  PARA-HINTAN2          PIC X(02).
     03  PARA-HINTAN3          PIC X(01).
     03  PARA-TANABAN          PIC X(06).
     03  PARA-JYUTYUSU         PIC 9(09).
     03  PARA-SYUKASU          PIC 9(09).
     03  PARA-GENKA            PIC 9(09).
     03  PARA-BAIKA            PIC 9(09).
     03  PARA-GENKAKIN         PIC 9(09).
     03  PARA-BAIKAKIN         PIC 9(09).
     03  PARA-AITECD           PIC X(13).
     03  PARA-HIKIATE          PIC X(01).
     03  PARA-UPBUMON          PIC X(04).
     03  PARA-UPTANCD          PIC X(02).
     03  PARA-JDATE            PIC 9(08).
     03  PARA-JTIME            PIC 9(04).
   01  PARA-BUMONCD            PIC X(04).
   01  PARA-TANTOCD            PIC X(02).
*#2018/03/15 NAV ED
*
 LINKAGE                SECTION.
 01  PARA-TORICD            PIC   9(08).
 01  PARA-DATE              PIC   9(08).
 01  PARA-TIME              PIC   9(04).
 01  PARA-NOBIS             PIC   9(08).
 01  PARA-NOBIE             PIC   9(08).
 01  PARA-KBN               PIC   X(01).
****************************************************************
 PROCEDURE              DIVISION USING  PARA-TORICD
                                        PARA-DATE
                                        PARA-TIME
                                        PARA-NOBIS
                                        PARA-NOBIE
                                        PARA-KBN.
****************************************************************
*--------------------------------------------------------------*
*    LEVEL 0        エラー処理　　　　　　　　　　　　　　　　 *
*--------------------------------------------------------------*
 DECLARATIVES.
*----<< 売上伝票ファイル >>--*
 SHTDENF-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SHTDENF.
     MOVE      "SHTDENF"    TO   AB-FILE.
     MOVE      SHTDENF-ST   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*----<< 出荷制限マスタ >>--*
 SYUSGNF-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      SYUSGNF.
     MOVE      "SYUSGNF"    TO   AB-FILE.
     MOVE      SYUSGNF-ST   TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*----<< 商品名称マスタ >>--*
 HMEIMS-ERR              SECTION.
     USE AFTER     EXCEPTION PROCEDURE      HMEIMS.
     MOVE      "MEIMS1 "    TO   AB-FILE.
     MOVE      HMEIMS-ST    TO   AB-STS.
     DISPLAY   MSG-ABEND         UPON CONS.
     DISPLAY   SEC-NAME          UPON CONS.
     DISPLAY   ABEND-FILE        UPON CONS.
     MOVE      4000         TO   PROGRAM-STATUS.
     STOP      RUN.
*----<< 自動欠品結果ファイル >>--*
 JIDKEKF-ERR             SECTION.
     USE AFTER     EXCEPTION PROCEDURE      JIDKEKF.
     MOVE      "JIDKEKF"    TO   AB-FILE.
     MOVE      JIDKEKF-ST   TO   AB-STS.
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
     DISPLAY "PARA-TORICD = " PARA-TORICD  UPON  CONS.
     DISPLAY "PARA-DATE   = " PARA-DATE    UPON  CONS.
     DISPLAY "PARA-TIME   = " PARA-TIME    UPON  CONS.
     DISPLAY "PARA-NOBIS  = " PARA-NOBIS   UPON  CONS.
     DISPLAY "PARA-NOBIE  = " PARA-NOBIE   UPON  CONS.
     DISPLAY "PARA-KBN    = " PARA-KBN     UPON  CONS.
*----<< ｼｮｷﾒｯｾｰｼﾞ ｼｭﾂﾘｮｸ >>--*
     DISPLAY  MSG-START UPON CONS.
*
*----<<FILE OPEN >>--*
     OPEN     I-O       SHTDENF.
     OPEN     INPUT     SYUSGNF  HMEIMS.
     OPEN     I-O       JIDKEKF.
*
*----<< ﾍﾝｽｳ ｸﾘｱ >--*
     MOVE     ZERO      TO        END-FG    RD-CNT    WRT-CNT.
     MOVE     ZERO      TO        IN-CNT    OUT-CNT.
     MOVE     SPACE     TO        DEN-REC.
     INITIALIZE                   DEN-REC.
*
*----<< キー情報セット >>--*
     MOVE     PARA-DATE       TO   DEN-F46.
     MOVE     PARA-TIME       TO   DEN-F47.
     MOVE     PARA-TORICD     TO   DEN-F01.

*
*----<< FILE START >>--*
     START    SHTDENF  KEY  >=   DEN-F46
                                 DEN-F47
                                 DEN-F01
                                 DEN-F48
                                 DEN-F02
                                 DEN-F04
                                 DEN-F051
                                 DEN-F07
                                 DEN-F112
                                 DEN-F03
        INVALID   KEY
           MOVE      9    TO   END-FG
             GO   TO   INIT-EXIT
     END-START.
*
*----<< １レコード目読込 >>--*
     PERFORM  900-DEN-READ.

 INIT-EXIT.
     EXIT.
****************************************************************
*　　　　　　　メイン処理　　　　　　　　　　　　　　　　　　　*
****************************************************************
 MAIN-SEC     SECTION.
*
     MOVE    "MAIN-SEC"           TO   S-NAME.
*
*----<< 変数クリア >>--*
     MOVE      SPACE     TO      TAISHO-FLG.
*
*----<< 自動欠品結果ファイル読込 >>--*
     MOVE     DEN-F46         TO   JID-F46.
     MOVE     DEN-F47         TO   JID-F47.
     MOVE     DEN-F01         TO   JID-F01.
     MOVE     DEN-F48         TO   JID-F48.
     MOVE     DEN-F02         TO   JID-F02.
     MOVE     DEN-F04         TO   JID-F04.
     MOVE     DEN-F051        TO   JID-F051.
     MOVE     DEN-F07         TO   JID-F07.
     MOVE     DEN-F112        TO   JID-F112.
     MOVE     DEN-F03         TO   JID-F03.
     PERFORM  900-JID-READ.
*
*----<< 自動欠品結果ファイルが存在しない場合 >>--*
     IF  JID-INVALID-FLG = " "
         MOVE     DEN-REC     TO   JID-REC
     END-IF.
*
*----<< 欠品区分によりチェック>>--*
     IF  PARA-KBN  =  SPACE   *>マスタ内容で欠品
         PERFORM  KEPKBN1-SEC
     END-IF.
*----<< 欠品区分によりチェック>>--*
     IF  PARA-KBN  =  "1"     *>全欠品
         PERFORM  KEPKBN2-SEC
     END-IF.
*----<< 欠品区分によりチェック>>--*
     IF  PARA-KBN  =  "2"     *>出荷停止欠品
         PERFORM  KEPKBN3-SEC
     END-IF.
*
*----<< 自動欠品結果ファイル更新 >>--*
     IF  TAISHO-FLG  =  "1"
         MOVE     "1"      TO   JID-F70
         ADD      1        TO   WRT-CNT
     END-IF.
     IF  JID-INVALID-FLG = "1"
       THEN
         REWRITE    JID-REC
       ELSE
         WRITE      JID-REC
     END-IF.
*
*----<< 次レコード読込 >>--*
     PERFORM  900-DEN-READ.

 MAIN-EXIT.
     EXIT.
****************************************************************
*　　欠品区分＝１（マスタ内容で欠品の場合）　　　　　　　　　　*
****************************************************************
 KEPKBN1-SEC   SECTION.
*
*----<< 納品日範囲チェック >>--*
     IF  PARA-NOBIS  <=  DEN-F112
     AND PARA-NOBIE  >=  DEN-F112
*--------<< 出荷制限マスタ読込 >>--*
         MOVE     DEN-F01  TO   SYU-F01
         MOVE     DEN-F07  TO   SYU-F02
         MOVE     DEN-F25  TO   SYU-F03
         PERFORM  900-SYU-READ
*--------<< 出荷制限マスタが存在した場合 >>--*
         IF  SYU-INVALID-FLG = "1"
             PERFORM  900-ZAIKO-SUB
             MOVE     ZAIKO-HIKIATE-FLG
                            TO   DEN-F27D
             PERFORM  900-DEN-WRITE
              MOVE     "1"      TO   TAISHO-FLG
         END-IF
     END-IF.
*
 KEPKBN1-EXIT.
     EXIT.
****************************************************************
*　　欠品区分＝２（全欠品の場合）　　　　　　　　　　　　　　　*
****************************************************************
 KEPKBN2-SEC   SECTION.
*
*----<< 納品日範囲チェック >>--*
     IF  PARA-NOBIS  <=  DEN-F112
     AND PARA-NOBIE  >=  DEN-F112
*--------<< 指定バッチ、指定納品日範囲の場合 >>--*
         PERFORM  900-ZAIKO-SUB
         MOVE     ZAIKO-HIKIATE-FLG
                        TO   DEN-F27D
         PERFORM  900-DEN-WRITE
         MOVE     "1"      TO   TAISHO-FLG
     END-IF.
*
 KEPKBN2-EXIT.
     EXIT.
****************************************************************
*　　欠品区分＝３（出荷停止商品の場合）　　　　　　　　　　　　*
****************************************************************
 KEPKBN3-SEC   SECTION.
*
*----<< サカタ商品ＣＤチェック、空白の場合はＥＸＩＴへ >>--*
     IF  DEN-F1411  =  SPACE
     AND DEN-F1412  =  SPACE
         GO            TO   KEPKBN3-EXIT
     END-IF.
*----<< 商品名称マスタ読込 >>--*
     MOVE  DEN-F1411        TO   MEI-F011.
     MOVE  DEN-F1412(1:5)   TO   MEI-F0121.
     MOVE  DEN-F1412(6:2)   TO   MEI-F0122.
     MOVE  DEN-F1412(8:1)   TO   MEI-F0123.
     PERFORM  900-MEI-READ.
*----<< 商品名称マスタが存在で廃盤区分＝２の時 >>--*
     IF  MEI-INVALID-FLG = SPACE
     AND MEI-F08  =  "2"
         PERFORM  900-ZAIKO-SUB
         MOVE     ZAIKO-HIKIATE-FLG
                        TO   DEN-F27D
         PERFORM  900-DEN-WRITE
         MOVE     "1"      TO   TAISHO-FLG
     END-IF.
*
 KEPKBN3-EXIT.
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
     CLOSE     SHTDENF.
     CLOSE     SYUSGNF.
     CLOSE     HMEIMS.
     CLOSE     JIDKEKF.
*
*****IF  OUT-CNT = ZERO
*        MOVE      4001         TO   PROGRAM-STATUS
*****END-IF.
*
     STOP      RUN.
*
 END-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    売上伝票ファイル READ (SEQUENTIAL)           *
*--------------------------------------------------------------*
 900-DEN-READ           SECTION.
     MOVE     "900-DEN-READ"      TO   S-NAME.
     READ     SHTDENF
       AT END
           MOVE      9    TO   END-FG
           GO  TO  900-DEN-READ-EXIT
       NOT AT END
           ADD       1    TO   RD-CNT
     END-READ.
*
     IF (DEN-F46  = PARA-DATE AND
         DEN-F47  = PARA-TIME AND
         DEN-F01  = PARA-TORICD )
     THEN
         CONTINUE
     ELSE
           MOVE      9    TO   END-FG
           GO  TO  900-DEN-READ-EXIT
     END-IF.
*
*----<< 売上計上済は読み飛ばし >>--*
     IF  DEN-F277  =  9
         GO  TO  900-DEN-READ
     END-IF.
 900-DEN-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    売上伝票ファイル WRITE                       *
*--------------------------------------------------------------*
 900-DEN-WRITE          SECTION.
*
*----<< 処理名セット >>--*
     MOVE     "900-DEN-WRITE"     TO   S-NAME.
*
*----<< 更新項目セット >>--*
*数量
     MOVE     ZERO                TO   DEN-F15.
*原価金額
     MOVE     ZERO                TO   DEN-F181.
*売価金額
     MOVE     ZERO                TO   DEN-F182.
*消費税
     MOVE     ZERO                TO   DEN-F19.
*粗利
     MOVE     ZERO                TO   DEN-F20.
*＃2018/03/15 NAV ST 受注残Ｆ更新
*****2018/03/15 NAV ST
     PERFORM  JYUZANF-UPD-SEC.
*****2018/03/15 NAV ED
*
*----<< 売上伝票ファイル更新 >>--*
     REWRITE    DEN-REC.
*
 900-DEN-WRITE-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    出荷制限マスタ READ (RANDOM)                 *
*--------------------------------------------------------------*
 900-SYU-READ           SECTION.
     MOVE     "900-SYU-READ"      TO   S-NAME.
     READ     SYUSGNF
       INVALID
              MOVE      SPACE          TO   SYU-INVALID-FLG
       NOT INVALID
              MOVE      "1"            TO   SYU-INVALID-FLG
     END-READ.
 900-SYU-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    自動欠品結果ファイル READ (RANDOM)           *
*--------------------------------------------------------------*
 900-JID-READ           SECTION.
     MOVE     "900-JID-READ"      TO   S-NAME.
     READ     JIDKEKF
       INVALID
              MOVE      SPACE          TO   JID-INVALID-FLG
       NOT INVALID
              MOVE      "1"            TO   JID-INVALID-FLG
     END-READ.
 900-JID-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    商品名称マスタREAD(RANDOM)                   *
*--------------------------------------------------------------*
 900-MEI-READ           SECTION.
     MOVE     "900-MEI-READ"      TO   S-NAME.
     READ     HMEIMS
       INVALID
              MOVE      "1"            TO   MEI-INVALID-FLG
       NOT INVALID
              MOVE      SPACE          TO   MEI-INVALID-FLG
     END-READ.
 900-MEI-READ-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    LEVEL ALL    在庫更新サブルーチン                         *
*--------------------------------------------------------------*
 900-ZAIKO-SUB          SECTION.
*
*----<< 変数クリア >>--*
     INITIALIZE                   ZAIKO-UPDATE-AREA.
*
*----<< 項目セット >>--*
*****DISPLAY "DEN-F01 = " DEN-F01 " - DEN-F02 = " DEN-F02 UPON
*****         CONS.
     MOVE     "1"                 TO   ZAIKO-MODE.
     MOVE     DEN-F01             TO   ZAIKO-TORICD.
     MOVE     DEN-F25             TO   ZAIKO-AITESHOHINCD.
     MOVE     DEN-F08             TO   ZAIKO-SOKOCD.
     MOVE     DEN-F1411           TO   ZAIKO-SHOHINCD.
     MOVE     DEN-F1412           TO   ZAIKO-HINTANCD.
     MOVE     DEN-F112            TO   ZAIKO-NOHINBI.
     MOVE     DEN-F15             TO   HENKOMAE-SURYO.
     MOVE     ZERO                TO   HENKOGO-SURYO.
     MOVE     DEN-F49             TO   ZAIKO-TANABAN.
     MOVE     DEN-F27D            TO   ZAIKO-HIKIATE-FLG.
*
*----<< サブルーチンコール >>--*
     CALL    "SJK9010B" USING     ZAIKO-MODE
                                  ZAIKO-TORICD
                                  ZAIKO-AITESHOHINCD
                                  ZAIKO-SOKOCD
                                  ZAIKO-SHOHINCD
                                  ZAIKO-HINTANCD
                                  ZAIKO-NOHINBI
                                  HENKOMAE-SURYO
                                  HENKOGO-SURYO
                                  ZAIKO-TANABAN
                                  ZAIKO-HIKIATE-FLG.
 900-ZAIKO-SUB-EXIT.
     EXIT.
*--------------------------------------------------------------*
*    受注残ファイル更新処理
*--------------------------------------------------------------*
 JYUZANF-UPD-SEC        SECTION.
*
     MOVE     "JYUZANF-UPD-SEC"        TO   S-NAME.
*
     MOVE      "2"                     TO   PARA-KOSINKBN.
     MOVE      "1"                     TO   PARA-DTKBN.
     MOVE      DEN-F01                 TO   PARA-TOKCD.
     MOVE      DEN-F02                 TO   PARA-DENNO.
     MOVE      DEN-F03                 TO   PARA-GYO.
     MOVE      DEN-F04                 TO   PARA-SOUSAI.
     MOVE      DEN-F051                TO   PARA-DENKU.
     MOVE      DEN-F07                 TO   PARA-TENCD.
     MOVE      DEN-F08                 TO   PARA-SOKCD.
     MOVE      DEN-F111                TO   PARA-HATYU.
     MOVE      DEN-F112                TO   PARA-NOUHIN.
     MOVE      DEN-F113                TO   PARA-SYUKA.
     MOVE      DEN-F1411               TO   PARA-SAKATACD.
     MOVE      DEN-F1412(1:5)          TO   PARA-HINTAN1.
     MOVE      DEN-F1412(6:2)          TO   PARA-HINTAN2.
     MOVE      DEN-F1412(8:1)          TO   PARA-HINTAN3.
     MOVE      DEN-F49                 TO   PARA-TANABAN.
     MOVE      DEN-F50                 TO   PARA-JYUTYUSU.
     MOVE      DEN-F15                 TO   PARA-SYUKASU.
     MOVE      DEN-F172                TO   PARA-GENKA.
     MOVE      DEN-F173                TO   PARA-BAIKA.
     MOVE      DEN-F181                TO   PARA-GENKAKIN.
     MOVE      DEN-F182                TO   PARA-BAIKAKIN.
     MOVE      DEN-F25                 TO   PARA-AITECD.
     MOVE      DEN-F27D                TO   PARA-HIKIATE.
     MOVE     "2920"                   TO   PARA-UPBUMON
                                            PARA-BUMONCD.
     MOVE     "99"                     TO   PARA-UPTANCD
                                            PARA-TANTOCD.
     MOVE      DEN-F46                 TO   PARA-JDATE.
     MOVE      DEN-F47                 TO   PARA-JTIME.
*サブルーチンコール
     CALL    "SJZ0110B" USING PARA-AREA
                              PARA-BUMONCD
                              PARA-TANTOCD.
*
 JYUZANF-UPD-EXIT.
     EXIT.
*-------------< PROGRAM END >----------------------------------*
