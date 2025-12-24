****************************************************************
*                                                              *
*    顧客名　　　　　　　：　（株）サカタのタネ殿　　　　　　　*
*    業務名　　　　　　　：　マスタメンテ　　　　　　　　　　　*
*    モジュール名　　　　：　商品名称マスタ（ＪＡＮＣＤ）　　　*
*    作成日／更新日　　　：　2005/10/25                        *
*    作成者／更新者　　　：　ＮＡＶ　　　　　　　　　　　　　　*
*    処理概要　　　　　　：　商品名称マスタをＪＡＮＣＤで検索  *
*                            する。　                          *
*                                                              *
****************************************************************
 IDENTIFICATION         DIVISION.
****************************************************************
 PROGRAM-ID.            SKE0502I.
 AUTHOR.                NAV.
****************************************************************
 ENVIRONMENT            DIVISION.
****************************************************************
 CONFIGURATION          SECTION.
 SOURCE-COMPUTER.       FACOM-K150.
 OBJECT-COMPUTER.       FACOM-K150.
 SPECIAL-NAMES.
         STATION   IS   STAT
         CONSOLE   IS   CONS.
*
 INPUT-OUTPUT           SECTION.
 FILE-CONTROL.
*---<<  画面ファイル  >>---*
     SELECT   DSPF      ASSIGN    TO        GS-DSPF
                        ORGANIZATION        IS   SEQUENTIAL
                        ACCESS    MODE      IS   SEQUENTIAL
                        SYMBOLIC  DESTINATION    IS  "DSP"
                        PROCESSING MODE     IS   DSP-PROC
                        GROUP               IS   DSP-GROUP
                        FORMAT              IS   DSP-FORMAT
                        SELECTED  FUNCTION  IS   DSP-FUNC
                        DESTINATION-1       IS   DSP-WSNO
                        FILE      STATUS    IS   DSP-STATUS.
*
*---<<  条件ファイル  >>---*
     SELECT   HJYOKEN   ASSIGN    TO        DA-01-VI-JYOKEN1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   JYO-F01
                                                 JYO-F02
                        FILE      STATUS    IS   JYO-STATUS.
*---<<  商品名称マスタ  >>---*
     SELECT   HMEIMS    ASSIGN    TO        DA-01-VI-MEIMS5
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   DYNAMIC
                        RECORD    KEY       IS   MEI-F06
                                                 MEI-F011
                                                 MEI-F0121
                                                 MEI-F0122
                                                 MEI-F0123
                        FILE      STATUS    IS   MEI-STATUS.
*---<<  仕入先マスタ  >>---*
     SELECT   ZSHIMS    ASSIGN    TO        DA-01-VI-ZSHIMS1
                        ORGANIZATION        IS   INDEXED
                        ACCESS    MODE      IS   RANDOM
                        RECORD    KEY       IS   SHI-F01
                        FILE      STATUS    IS   SHI-STATUS.
*
 DATA                   DIVISION.
 FILE                   SECTION.
*---<<  画面ファイル  >>---*
 FD  DSPF.
 01  DSP-REC            PIC  X(2000).
     COPY     FKE05002  OF        XMDLIB.
*---<<  条件ファイル  >>---*
 FD  HJYOKEN.
     COPY     HJYOKEN   OF        XFDLIB
              JOINING   JYO       PREFIX.
*---<<  商品名称マスタ  >>---*
 FD  HMEIMS.
     COPY     HMEIMS    OF        XFDLIB
              JOINING   MEI       PREFIX.
*---<<  仕入先マスタ  >>---*
 FD  ZSHIMS.
     COPY     ZSHIMS    OF        XFDLIB
              JOINING   SHI       PREFIX.
****  作業領域  ***
 WORKING-STORAGE             SECTION.
****  画面制御項目  ****
 01  DSP-CONTROL.
     03  DSP-PROC            PIC  X(02).
     03  DSP-GROUP           PIC  X(08).
     03  DSP-FORMAT          PIC  X(08).
     03  DSP-STATUS          PIC  X(02).
     03  DSP-FUNC            PIC  X(04).
     03  DSP-WSNO            PIC  X(08).
****  ステイタス情報  ***
 01  STATUS-AREA.
     02  JYO-STATUS          PIC  X(02).
     02  MEI-STATUS          PIC  X(02).
     02  SHI-STATUS          PIC  X(02).
****  フラグ  ***
 01  PSW-AREA.
     02  END-FLG             PIC  X(03)  VALUE SPACE.
     02  READ-FLG            PIC  X(01)  VALUE SPACE.
     02  MAIN-FLG            PIC  X(01)  VALUE SPACE.
     02  INVALID-FLG         PIC  9(01)  VALUE ZERO.
     02  MEI-FLG             PIC  9(01)  VALUE ZERO.
****  処理スイッチ  ****
 01  WK-AREA.
****  インデックス  ****
     02  IXA                 PIC  9(02).
     02  IXB                 PIC  9(02).
****  システム日付  ****
*日付／時刻
 01  TIME-AREA.
     03  WK-TIME                  PIC  9(08)  VALUE  ZERO.
 01  DATE-AREA.
     03  WK-YS                    PIC  9(02)  VALUE  ZERO.
     03  WK-DATE.
         05  WK-Y                 PIC  9(02)  VALUE  ZERO.
         05  WK-M                 PIC  9(02)  VALUE  ZERO.
         05  WK-D                 PIC  9(02)  VALUE  ZERO.
 01  DATE-AREAR2       REDEFINES      DATE-AREA.
     03  SYS-DATE2                PIC  9(08).
*特販部名称編集
 01  HEN-TOKHAN-AREA.
     03  FILLER                   PIC  N(01)  VALUE  NC"（".
     03  HEN-TOKHAN               PIC  N(06)  VALUE  SPACE.
     03  FILLER                   PIC  N(01)  VALUE  NC"）".
*画面表示日付編集
 01  HEN-DATE.
     03  HEN-DATE-YYYY            PIC  9(04)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  "/".
     03  HEN-DATE-DD              PIC  9(02)  VALUE  ZERO.
*画面表示時刻編集
 01  HEN-TIME.
     03  HEN-TIME-HH              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-MM              PIC  9(02)  VALUE  ZERO.
     03  FILLER                   PIC  X(01)  VALUE  ":".
     03  HEN-TIME-SS              PIC  9(02)  VALUE  ZERO.
*
 01  SAV-KEY.
     03  SAV-BKEY.
       05  SAV-BKEY-JAN        PIC  X(13)   VALUE  SPACE.
       05  SAV-BKEY-SHO        PIC  X(08)   VALUE  SPACE.
       05  SAV-BKEY-HIN        PIC  X(08)   VALUE  SPACE.
     03  SAV-NKEY.
       05  SAV-NKEY-JAN        PIC  X(13)   VALUE  SPACE.
       05  SAV-NKEY-SHO        PIC  X(08)   VALUE  SPACE.
       05  SAV-NKEY-HIN        PIC  X(08)   VALUE  SPACE.
*
****  JAN右詰め用エリア      ****
 01  WK-JANCD.
     03  WK-JAN              PIC  X(01)   OCCURS  13.
*
****  ＰＦキーガイド  ***
 01  MSG-AREA.
     02  PMSG01            PIC N(07) VALUE
             NC"_取消　_終了".
     02  PMSG02            PIC N(19) VALUE
             NC"_取消　_終了　_戻り　_前頁　_次頁".
     02  PMSG03            PIC N(11) VALUE
             NC"_取消　_終了　_戻り".
****  メッセージ情報  ***
 01  MSG-AREA1-1.
     02  MSG-ABEND1.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-PG-ID         PIC  X(08)  VALUE  "SKE0502I".
       03  FILLER            PIC  X(10)  VALUE  " ABEND ###".
     02  MSG-ABEND2.
       03  FILLER            PIC  X(04)  VALUE  "### ".
       03  ERR-FL-ID         PIC  X(08).
       03  FILLER            PIC  X(04)  VALUE  " ST-".
       03  ERR-STCD          PIC  X(02).
       03  FILLER            PIC  X(04)  VALUE  " ###".
****  エラーメッセージコード  ***
 01  CODE-AREA.
     02  ERR-MSG-CD          PIC  9(02)  VALUE  ZERO.
****  エラーメッセージ  ***
 01  ERR-TAB.
     02  MSG-ERR1            PIC  N(20)  VALUE
             NC"無効ＰＦキーです。".
     02  MSG-ERR2            PIC  N(20)  VALUE
            NC"仕入先マスタに存在しません".
     02  MSG-ERR3            PIC  N(20)  VALUE
             NC"入力項目に間違いがあります".
     02  MSG-ERR4            PIC  N(20) VALUE
            NC"Ｙ，Ｈ，Ｂのいずれかで入力して下さい".
     02  MSG-ERR5            PIC  N(20) VALUE
            NC"ＹかＨで入力して下さい".
     02  MSG-ERR6            PIC  N(20) VALUE
            NC"Ｙで入力して下さい".
     02  MSG-ERR7            PIC  N(20)  VALUE
             NC"該当データがありません".
     02  MSG-ERR8            PIC  N(20)  VALUE
             NC"前頁がありません".
     02  MSG-ERR9            PIC  N(20)  VALUE
             NC"次頁がありません".
     02  MSG-ERR10           PIC  N(20)  VALUE
             NC"自動発注区分に誤りがあります。".
     02  MSG-ERR11           PIC  N(20)  VALUE
             NC"定期区分に誤りがあります。".
     02  MSG-ERR12           PIC  N(20)  VALUE
             NC"季節区分を入力して下さい。".
     02  MSG-ERR13           PIC  N(20)  VALUE
             NC"季節区分に誤りがあります。".
     02  MSG-ERR14           PIC  N(20)  VALUE
             NC"廃盤区分に誤りがあります。".
     02  MSG-ERR15           PIC  N(20)  VALUE
             NC"仕入先マスタに未登録です。".
 01  ERR-MSG-ALL     REDEFINES    ERR-TAB.
     02  ERR-MSG             PIC  N(20)
                             OCCURS 15  TIMES.
*日付変換サブルーチン用ワーク
 01  LINK-IN-KBN           PIC X(01).
 01  LINK-IN-YMD6          PIC 9(06).
 01  LINK-IN-YMD8          PIC 9(08).
 01  LINK-OUT-RET          PIC X(01).
 01  LINK-OUT-YMD          PIC 9(08).
*----------------------------------------------------------*
*             ＭＡＩＮ         ＭＯＤＵＬＥ                *
*----------------------------------------------------------*
 PROCEDURE              DIVISION.
**
 DECLARATIVES.
 FILEERR-DSP            SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   DSPF.
     MOVE     "DSPF    "       TO   ERR-FL-ID.
     MOVE      DSP-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
**
 FILEERR-JYO            SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HJYOKEN.
     MOVE     "HJYOKEN"        TO   ERR-FL-ID.
     MOVE      JYO-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
**
 FILEERR-HMEI           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   HMEIMS.
     MOVE     "HMEIMS "        TO   ERR-FL-ID.
     MOVE      MEI-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
 FILEERR-ZSHI           SECTION.
     USE       AFTER    EXCEPTION
                        PROCEDURE   ZSHIMS.
     MOVE     "ZSHIMS "        TO   ERR-FL-ID.
     MOVE      SHI-STATUS      TO   ERR-STCD.
     DISPLAY   MSG-ABEND1    UPON   CONS.
     DISPLAY   MSG-ABEND2    UPON   CONS.
     MOVE      4000            TO   PROGRAM-STATUS.
     STOP      RUN.
**
 END     DECLARATIVES.
************************************************************
 SKE0502I-START         SECTION.
     PERFORM      INIT-SEC.
     PERFORM      MAIN-SEC
                  UNTIL     END-FLG  =    "END".
     PERFORM      END-SEC.
     STOP      RUN.
 SKE0502I-END.
     EXIT.
************************************************************
*      _０     初期処理                                   *
************************************************************
 INIT-SEC               SECTION.
     OPEN     I-O       DSPF      HMEIMS.
     OPEN     INPUT     HJYOKEN   ZSHIMS.
*
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
*画面表示日付編集
     MOVE      SYS-DATE2(1:4)      TO   HEN-DATE-YYYY.
     MOVE      SYS-DATE2(5:2)      TO   HEN-DATE-MM.
     MOVE      SYS-DATE2(7:2)      TO   HEN-DATE-DD.
*システム日付取得
     ACCEPT    WK-TIME          FROM   TIME.
*画面表示時刻編集
     MOVE      WK-TIME(1:2)       TO   HEN-TIME-HH.
     MOVE      WK-TIME(3:2)       TO   HEN-TIME-MM.
     MOVE      WK-TIME(5:2)       TO   HEN-TIME-SS.
*特販部名称編集
     MOVE    SPACE               TO        JYO-REC.
     INITIALIZE                            JYO-REC.
     MOVE    "99"                TO        JYO-F01.
     MOVE    "BUMON"             TO        JYO-F02.
     READ    HJYOKEN
       INVALID KEY
             MOVE NC"＊＊＊＊＊＊"   TO    HEN-TOKHAN
       NOT INVALID KEY
             MOVE JYO-F03            TO    HEN-TOKHAN
     END-READ.
     MOVE    HEN-TOKHAN-AREA      TO   TOKHAN.
*
     MOVE    "1"             TO   MAIN-FLG.
*
     MOVE     SPACE          TO   FKE05002.
     PERFORM       DSP-WRITE-SUB.
     PERFORM       EDIT-SET-HEAD.
     PERFORM       EDIT-SET-BODY.
 INIT-END.
     EXIT.
************************************************************
*      _０      メイン処理                                *
************************************************************
 MAIN-SEC          SECTION.
     EVALUATE      MAIN-FLG
         WHEN      "1"  PERFORM   HEAD-SUB
*********WHEN      "2"  PERFORM   BODY-SUB
         WHEN      "3"  PERFORM   KAKUNIN-SUB
         WHEN           OTHER     CONTINUE
     END-EVALUATE.
 MAIN-END.
     EXIT.
*==========================================================*
*      2.1       ヘッド部入力              MAIN-FLG=1      *
*==========================================================*
 HEAD-SUB            SECTION.
     PERFORM         MSG-SEC.
     MOVE     PMSG01         TO   PFMSG.
     PERFORM         DSP-WRITE-SUB.
     MOVE    "HEAD"          TO   DSP-GROUP.
     PERFORM         DSP-READ-SUB.
* アテンション判定
     EVALUATE    DSP-FUNC
         WHEN   "F004"
                        MOVE    "1"    TO   MAIN-FLG
                        MOVE     SPACE TO   FKE05002
                        PERFORM             HEADDEL-SUB
                        PERFORM             BODYDEL-SUB
         WHEN   "F005"
                        MOVE     "END" TO   END-FLG
         WHEN   "E000"
                        PERFORM   BODYDEL-SUB
                        PERFORM   MEIMS-DSP-SUB
                        IF   ERR-MSG-CD     =    ZERO
                             MOVE     "3"   TO   MAIN-FLG
                        END-IF
              MOVE   " "     TO   EDIT-OPTION  OF  JAN
              MOVE   "C"     TO   EDIT-CURSOR  OF  JAN
         WHEN    OTHER
                        MOVE      01   TO   ERR-MSG-CD
     END-EVALUATE.
 HEAD-END.
     EXIT.
*==========================================================*
*      2.1.1     明細セット処理                            *
*==========================================================*
 MEIMS-DSP-SUB       SECTION.
*
     PERFORM  EDIT-SET-BODY.
     MOVE     ZERO      TO   ERR-MSG-CD.
*    JANコード右詰
*****IF  JAN       NOT =     SPACE
*********MOVE      JAN       TO   WK-JANCD
*********PERFORM        UNTIL     WK-JAN(13) NOT = SPACE
**************PERFORM VARYING IXA FROM 12 BY -1 UNTIL IXA = 0
*******************MOVE      WK-JAN(IXA)    TO  WK-JAN(IXA + 1)
**************END-PERFORM
**************MOVE      ZERO      TO  WK-JAN(1)
*********END-PERFORM
*********MOVE      WK-JANCD       TO   JAN
*****END-IF.
*
     PERFORM  MEIMS-START-SUB.
     IF       MEI-FLG   =    ZERO
              PERFORM   MEIMS-READ-SUB
     ELSE
              IF   ERR-MSG-CD     =    ZERO
                   MOVE      7    TO   ERR-MSG-CD
              END-IF
              MOVE   "R"     TO   EDIT-OPTION  OF  JAN
              MOVE   "C"     TO   EDIT-CURSOR  OF  JAN
              GO      TO     MEIMS-DSP-EXIT
     END-IF.
     PERFORM  VARYING   IXA  FROM  1  BY  1
              UNTIL   ( IXA       >    8 )  OR
                      ( MEI-FLG   =    1 )
*        JANCD
         MOVE      MEI-F06        TO   JANCD (IXA)
*        商品ＣＤ
         MOVE      MEI-F011       TO   SHOCD (IXA)
*        品単ＣＤ
         MOVE      MEI-F0121      TO   MHIN1 (IXA)
         MOVE      MEI-F0122      TO   MHIN2 (IXA)
         MOVE      MEI-F0123      TO   MHIN3 (IXA)
*        商品名称
         MOVE      MEI-F02        TO   HINMEI(IXA)
*        入数
         MOVE      MEI-F07        TO   IRISU (IXA)
*        仕入先名
         MOVE      MEI-F05        TO   SHI-F01
         PERFORM   SHI-READ-SUB
         IF   INVALID-FLG    =    1
              MOVE ALL   NC"＊"        TO   SIIREN(IXA)
         ELSE
              MOVE       SHI-F02       TO   SIIREN(IXA)
         END-IF
*
         IF        IXA  =    1
              MOVE MEI-F06        TO   SAV-BKEY-JAN
         END-IF
         IF        IXA  =    8
              MOVE MEI-F06        TO   SAV-NKEY-JAN
         END-IF
*
         PERFORM   MEIMS-READ-SUB
     END-PERFORM.
*
 MEIMS-DSP-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.1.1.1   商品名称マスタＳＴＡＲＴ                  *
*----------------------------------------------------------*
 MEIMS-START-SUB        SECTION.
*
     MOVE    ZERO            TO   MEI-FLG.
     MOVE    SPACE           TO   MEI-REC.
     INITIALIZE                   MEI-REC.
     MOVE    JAN             TO   MEI-F06.
     START   HMEIMS     KEY  >=   MEI-F06
       INVALID      KEY
             MOVE       1         TO   MEI-FLG
       NOT INVALID  KEY
             MOVE       ZERO      TO   MEI-FLG
     END-START.
 MEIMS-START-END.
     EXIT.
*----------------------------------------------------------*
*      2.1.1.2   商品名称マスタＲＥＡＤ                    *
*----------------------------------------------------------*
 MEIMS-READ-SUB        SECTION.
*
     READ    HMEIMS    NEXT
         AT END
             MOVE      1     TO   MEI-FLG
         NOT AT END
             MOVE      ZERO  TO   MEI-FLG
     END-READ.
*
 MEIMS-START-END.
     EXIT.
*==========================================================*
*      2.2       ボディ部入力              MAIN-FLG=2      *
*==========================================================*
 BODY-SUB            SECTION.
     PERFORM         MSG-SEC.
     MOVE     PMSG02         TO   PFMSG.
     PERFORM         DSP-WRITE-SUB.
     MOVE   "BODY"           TO   DSP-GROUP.
     PERFORM         DSP-READ-SUB.
* アテンション判定
     EVALUATE    DSP-FUNC
         WHEN   "F004"
                        MOVE    "1"    TO   MAIN-FLG
                        MOVE     SPACE TO   FKE05002
                        PERFORM             HEADDEL-SUB
                        PERFORM             BODYDEL-SUB
         WHEN   "F005"
                        MOVE     "END" TO   END-FLG
         WHEN   "F006"
                        PERFORM             EDIT-SET-BODY
                        MOVE    "1"    TO   MAIN-FLG
         WHEN   "F011"
                        PERFORM   BODYCHK-SUB
                        IF  ERR-MSG-CD     =    ZERO
                            PERFORM   MEIMS-UPD-SUB
                            PERFORM   BEFORE-PAGE-SUB
                        END-IF
         WHEN   "F012"
                        PERFORM   BODYCHK-SUB
                        IF  ERR-MSG-CD     =    ZERO
                            PERFORM   MEIMS-UPD-SUB
                            PERFORM   NEXT-PAGE-SUB
                        END-IF
         WHEN   "E000"
                        PERFORM   BODYCHK-SUB
                        IF  ERR-MSG-CD     =    ZERO
                            MOVE   "3"     TO   MAIN-FLG
                        END-IF
         WHEN    OTHER
                        MOVE   01          TO   ERR-MSG-CD
     END-EVALUATE.
 BODY-END.
     EXIT.
*----------------------------------------------------------*
*      2.4.1     ボディ部の入力チェック                    *
*----------------------------------------------------------*
 BODYCHK-SUB            SECTION.
     PERFORM  EDIT-SET-BODY.
     MOVE     ZERO      TO   ERR-MSG-CD.
*
     PERFORM  VARYING   IXA  FROM  1  BY  1
              UNTIL     IXA    >   8
         IF   SHOCD (IXA) =  LOW-VALUE
              MOVE   SPACE      TO   SHOCD(IXA)
         END-IF
         IF   MHIN1  (IXA) =  LOW-VALUE
              MOVE   SPACE      TO   MHIN1 (IXA)
         END-IF
         IF   MHIN2  (IXA) =  LOW-VALUE
              MOVE   SPACE      TO   MHIN2 (IXA)
         END-IF
         IF   MHIN3  (IXA) =  LOW-VALUE
              MOVE   SPACE      TO   MHIN3 (IXA)
         END-IF
     END-PERFORM.
*
 BODYCHK-END.
     EXIT.
*----------------------------------------------------------*
*      2.4.2     前頁処理                                  *
*----------------------------------------------------------*
 BEFORE-PAGE-SUB     SECTION.
*
     PERFORM  MEIMS-START3-SUB.
     IF       MEI-FLG   =    ZERO
              PERFORM   MEIMS-READ-SUB
              IF   MEI-FLG   NOT =     ZERO
                   IF   ERR-MSG-CD     =    ZERO
                        MOVE      8    TO   ERR-MSG-CD
                   END-IF
                   GO   TO   BEFORE-PAGE-EXIT
              END-IF
     ELSE
              IF   ERR-MSG-CD     =    ZERO
                   MOVE      8    TO   ERR-MSG-CD
              END-IF
              GO   TO   BEFORE-PAGE-EXIT
     END-IF.
     MOVE     SPACE     TO   SAV-BKEY.
     PERFORM  BODYDEL-SUB.
     PERFORM  VARYING   IXA  FROM  8  BY  -1
              UNTIL   ( IXA       =    0 )  OR
                      ( MEI-FLG   =    1 )
         MOVE      MEI-F011       TO   SHOCD (IXA)
         MOVE      MEI-F0121      TO   MHIN1 (IXA)
         MOVE      MEI-F0122      TO   MHIN2 (IXA)
         MOVE      MEI-F0123      TO   MHIN3 (IXA)
         MOVE      MEI-F02        TO   HINMEI(IXA)
*        JANCD
         MOVE      MEI-F06        TO   JANCD (IXA)
*        入数
         MOVE      MEI-F07        TO   IRISU (IXA)
*        仕入先名
         MOVE      MEI-F05        TO   SHI-F01
         PERFORM   SHI-READ-SUB
         IF   INVALID-FLG    =    1
              MOVE ALL   NC"＊"        TO   SIIREN(IXA)
         ELSE
              MOVE       SHI-F02       TO   SIIREN(IXA)
         END-IF
*
         IF        IXA  =    1
              MOVE MEI-F06        TO   SAV-BKEY-JAN
              MOVE MEI-F011       TO   SAV-BKEY-SHO
              MOVE MEI-F012       TO   SAV-BKEY-HIN
         END-IF
         IF        IXA  =    8
              MOVE MEI-F06        TO   SAV-NKEY-JAN
              MOVE MEI-F011       TO   SAV-NKEY-SHO
              MOVE MEI-F012       TO   SAV-NKEY-HIN
         END-IF
*
         PERFORM   MEIMS-READ-SUB
     END-PERFORM.
*
 BEFORE-PAGE-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.4.3     次頁処理                                  *
*----------------------------------------------------------*
 NEXT-PAGE-SUB     SECTION.
*
     PERFORM  MEIMS-START2-SUB.
     IF       MEI-FLG   =    ZERO
              PERFORM   MEIMS-READ-SUB
              IF   MEI-FLG   NOT =     ZERO
                   IF   ERR-MSG-CD     =    ZERO
                        MOVE      9    TO   ERR-MSG-CD
                   END-IF
                   GO   TO   NEXT-PAGE-EXIT
              END-IF
     ELSE
              IF   ERR-MSG-CD     =    ZERO
                   MOVE      9    TO   ERR-MSG-CD
              END-IF
              GO   TO   NEXT-PAGE-EXIT
     END-IF.
     MOVE     SPACE     TO   SAV-NKEY.
     PERFORM  BODYDEL-SUB.
     PERFORM  VARYING   IXA  FROM  1  BY  1
              UNTIL   ( IXA       >    8 )  OR
                      ( MEI-FLG   =    1 )
         MOVE      MEI-F011       TO   SHOCD (IXA)
         MOVE      MEI-F0121      TO   MHIN1 (IXA)
         MOVE      MEI-F0122      TO   MHIN2 (IXA)
         MOVE      MEI-F0123      TO   MHIN3 (IXA)
         MOVE      MEI-F02        TO   HINMEI(IXA)
*        JANCD
         MOVE      MEI-F06        TO   JANCD (IXA)
*        入数
         MOVE      MEI-F07        TO   IRISU (IXA)
*        仕入先名
         MOVE      MEI-F05        TO   SHI-F01
         PERFORM   SHI-READ-SUB
         IF   INVALID-FLG    =    1
              MOVE ALL   NC"＊"        TO   SIIREN(IXA)
         ELSE
              MOVE       SHI-F02       TO   SIIREN(IXA)
         END-IF
*
         IF        IXA  =    1
              MOVE MEI-F06        TO   SAV-BKEY-JAN
              MOVE MEI-F011       TO   SAV-BKEY-SHO
              MOVE MEI-F012       TO   SAV-BKEY-HIN
         END-IF
         IF        IXA  =    8
              MOVE MEI-F06        TO   SAV-NKEY-JAN
              MOVE MEI-F011       TO   SAV-NKEY-SHO
              MOVE MEI-F012       TO   SAV-NKEY-HIN
         END-IF
*
         PERFORM   MEIMS-READ-SUB
     END-PERFORM.
*
 NEXT-PAGE-EXIT.
     EXIT.
*----------------------------------------------------------*
*      2.4.3     商品名称マスタＳＴＡＲＴ２                *
*----------------------------------------------------------*
 MEIMS-START2-SUB       SECTION.
*
     MOVE    ZERO            TO   MEI-FLG.
     MOVE    SPACE           TO   MEI-REC.
     INITIALIZE                   MEI-REC.
     MOVE    SAV-NKEY-JAN    TO   MEI-F06.
     MOVE    SAV-NKEY-SHO    TO   MEI-F011.
     MOVE    SAV-NKEY-HIN    TO   MEI-F012.
     START   HMEIMS     KEY  >    MEI-F06
                                  MEI-F011
                                  MEI-F0121
                                  MEI-F0122
                                  MEI-F0123
       INVALID      KEY
             MOVE       1         TO   MEI-FLG
       NOT INVALID  KEY
             MOVE       ZERO      TO   MEI-FLG
     END-START.
 MEIMS-START2-END.
     EXIT.
*----------------------------------------------------------*
*      2.4.4     商品名称マスタＳＴＡＲＴ３                *
*----------------------------------------------------------*
 MEIMS-START3-SUB       SECTION.
*
     MOVE    ZERO            TO   MEI-FLG.
     MOVE    SPACE           TO   MEI-REC.
     INITIALIZE                   MEI-REC.
     MOVE    SAV-BKEY-JAN    TO   MEI-F06.
     MOVE    SAV-BKEY-SHO    TO   MEI-F011.
     MOVE    SAV-BKEY-HIN    TO   MEI-F012.
     START   HMEIMS     KEY  <    MEI-F06
                                  MEI-F011
                                  MEI-F0121
                                  MEI-F0122
                                  MEI-F0123
                        WITH      REVERSED  ORDER
       INVALID      KEY
             MOVE       1         TO   MEI-FLG
       NOT INVALID  KEY
             MOVE       ZERO      TO   MEI-FLG
     END-START.
 MEIMS-START3-END.
     EXIT.
*----------------------------------------------------------*
*      2.5       確認入力                                  *
*----------------------------------------------------------*
 KAKUNIN-SUB       SECTION.
     PERFORM       MSG-SEC.
     MOVE     PMSG02         TO   PFMSG.
     PERFORM       DSP-WRITE-SUB.
     MOVE    "KAKU"          TO    DSP-GROUP.
     PERFORM       DSP-READ-SUB.
* アテンション判定
     EVALUATE  DSP-FUNC
         WHEN   "F004"
                        MOVE    "1"    TO   MAIN-FLG
                        PERFORM   HEADDEL-SUB
                        PERFORM   BODYDEL-SUB
         WHEN   "F005"
                        MOVE     "END" TO   END-FLG
         WHEN   "F006"
                        MOVE     "1"   TO   MAIN-FLG
         WHEN   "F011"
************************PERFORM   BODYCHK-SUB
                        IF  ERR-MSG-CD     =    ZERO
                            PERFORM   MEIMS-UPD-SUB
                            PERFORM   BEFORE-PAGE-SUB
                        END-IF
         WHEN   "F012"
************************PERFORM   BODYCHK-SUB
                        IF  ERR-MSG-CD     =    ZERO
                            PERFORM   MEIMS-UPD-SUB
                            PERFORM   NEXT-PAGE-SUB
                        END-IF
         WHEN   "E000"
                        PERFORM   KAKUCHK-SUB
         WHEN    OTHER
                        MOVE      01   TO   ERR-MSG-CD
     END-EVALUATE.
 KAKUNIN-END.
     EXIT.
*----------------------------------------------------------*
*      2.5.1     確認の入力チェック                        *
*----------------------------------------------------------*
 KAKUCHK-SUB             SECTION.
     MOVE       "M"     TO   EDIT-OPTION  OF  KAKU.
     MOVE       SPACE   TO   EDIT-CURSOR  OF  KAKU.
     MOVE       ZERO    TO   ERR-MSG-CD.
*
     PERFORM       MEIMS-UPD-SUB.
     PERFORM       HEADDEL-SUB.
     PERFORM       BODYDEL-SUB.
     MOVE    "1"   TO  MAIN-FLG.
*
 KAKUCHK-END.
     EXIT.
*----------------------------------------------------------*
*      2.5.1.1.2  商品名称マスタ更新                       *
*----------------------------------------------------------*
 MEIMS-UPD-SUB          SECTION.
*    商品名称マスタ更新
     PERFORM  VARYING   IXA       FROM    1  BY  1
              UNTIL     IXA  >    8
         IF   JANCD(IXA)     NOT =     SPACE
              PERFORM   MEI-READ-SUB
              IF   INVALID-FLG    =    1
                   CONTINUE
              ELSE
                   IF ( IRISU (IXA)    NOT =     MEI-F07 )
                        MOVE IRISU (IXA)    TO   MEI-F07
                        REWRITE   MEI-REC
                   END-IF
              END-IF
         END-IF
     END-PERFORM.
 MEIMS-UPD-END.
     EXIT.
************************************************************
*      3.0        終了処理                                 *
************************************************************
 END-SEC                SECTION.
     CLOSE    DSPF      HJYOKEN
              HMEIMS    ZSHIMS.
 END-END.
     EXIT.
************************************************************
*      9.0        共通処理                                 *
************************************************************
*==========================================================*
*      9.1       エラー メッセージセット                   *
*==========================================================*
 MSG-SEC                SECTION.
     IF  ERR-MSG-CD     =    ZERO
         MOVE      SPACE     TO   ERRMSG
     ELSE
         MOVE      ERR-MSG(ERR-MSG-CD)      TO   ERRMSG
         MOVE      ZERO      TO   ERR-MSG-CD
     END-IF.
 MSG-END.
     EXIT.
*==========================================================*
*      9.2       項目制御部初期化                          *
*==========================================================*
*----------------------------------------------------------*
*      9.2.1     項目制御部初期化（ヘッド部）              *
*----------------------------------------------------------*
 EDIT-SET-HEAD          SECTION.
     MOVE   "M"     TO   EDIT-OPTION  OF  JAN.
     MOVE   SPACE   TO   EDIT-CURSOR  OF  JAN.
 EDIT-SET-HEAD-END.
     EXIT.
*----------------------------------------------------------*
*      9.2.3     項目制御部初期化（ボディ部）              *
*----------------------------------------------------------*
 EDIT-SET-BODY          SECTION.
     PERFORM  VARYING IXB     FROM  1  BY  1
              UNTIL   IXB     >     8
         PERFORM     EDIT-SET-GYO
     END-PERFORM.
 EDIT-SET-BODY-END.
     EXIT.
*----------------------------------------------------------*
*      9.2.3.1   項目制御部初期化（ボディ部１行）          *
*----------------------------------------------------------*
 EDIT-SET-GYO           SECTION.
*
     MOVE   "M"     TO   EDIT-OPTION  OF  SHOCD (IXB).
     MOVE   SPACE   TO   EDIT-CURSOR  OF  SHOCD (IXB).
     MOVE   "M"     TO   EDIT-OPTION  OF  MHIN1 (IXB).
     MOVE   SPACE   TO   EDIT-CURSOR  OF  MHIN1 (IXB).
     MOVE   "M"     TO   EDIT-OPTION  OF  MHIN2 (IXB).
     MOVE   SPACE   TO   EDIT-CURSOR  OF  MHIN2 (IXB).
     MOVE   "M"     TO   EDIT-OPTION  OF  MHIN3 (IXB).
     MOVE   SPACE   TO   EDIT-CURSOR  OF  MHIN3 (IXB).
     MOVE   "M"     TO   EDIT-OPTION  OF  JANCD (IXB).
     MOVE   SPACE   TO   EDIT-CURSOR  OF  JANCD (IXB).
     MOVE   "M"     TO   EDIT-OPTION  OF  IRISU (IXB).
     MOVE   SPACE   TO   EDIT-CURSOR  OF  IRISU (IXB).
     MOVE   "M"     TO   EDIT-OPTION  OF  SIIREN(IXB).
     MOVE   SPACE   TO   EDIT-CURSOR  OF  SIIREN(IXB).
*
 EDIT-SET-GYO-END.
     EXIT.
*==========================================================*
*      9.3       画面表示処理                              *
*==========================================================*
 DSP-WRITE-SUB          SECTION.
     MOVE    "FKE05002"      TO   DSP-FORMAT.
     MOVE    "SCREEN"        TO   DSP-GROUP.
     MOVE     SPACE          TO   DSP-PROC.
     MOVE     HEN-DATE       TO   SDATE.
     MOVE     HEN-TIME       TO   STIME.
     MOVE     HEN-TOKHAN-AREA     TO   TOKHAN.
     WRITE    FKE05002.
 DSP-WRITE-END.
     EXIT.
*==========================================================*
*      9.4       画面データの入力処理                      *
*==========================================================*
 DSP-READ-SUB           SECTION.
     MOVE  "NE"    TO   DSP-PROC.
     READ   DSPF.
 DSP-READ-END.
     EXIT.
*==========================================================*
*      9.5       ＨＥＡＤ部消去                            *
*==========================================================*
 HEADDEL-SUB            SECTION.
     MOVE   SPACE       TO   MAS003.
     PERFORM            EDIT-SET-HEAD.
 HEADDEL-END.
     EXIT.
*==========================================================*
*      9.6       ＢＯＤＹ部消去                            *
*==========================================================*
 BODYDEL-SUB            SECTION.
     MOVE  SPACE        TO   MAS002.
     PERFORM            EDIT-SET-BODY.
 BODYDEL-END.
     EXIT.
*==========================================================*
*      9.7       ファイル処理                              *
*==========================================================*
*----------------------------------------------------------*
*      9.7.1     条件ファイルＲＥＡＤ                      *
*----------------------------------------------------------*
 JYO-READ-SUB           SECTION.
     READ    HJYOKEN
       INVALID      KEY
          MOVE      1        TO   INVALID-FLG
          INITIALIZE              JYO-REC
       NOT INVALID  KEY
          MOVE      ZERO     TO   INVALID-FLG
     END-READ.
 JYO-READ-END.
     EXIT.
*----------------------------------------------------------*
*      9.7.3     商品名称マスタの検索                      *
*----------------------------------------------------------*
 MEI-READ-SUB           SECTION.
     MOVE    JANCD (IXA)     TO   MEI-F06.
     READ    HMEIMS
       INVALID      KEY
          MOVE      1        TO   INVALID-FLG
       NOT INVALID  KEY
          MOVE     ZERO      TO   INVALID-FLG
     END-READ.
 MEI-READ-END.
     EXIT.
*----------------------------------------------------------*
*      9.7.4     仕入先マスタ読込　   　　                 *
*----------------------------------------------------------*
 SHI-READ-SUB           SECTION.
     READ    ZSHIMS
       INVALID      KEY
          MOVE      1        TO   INVALID-FLG
       NOT INVALID  KEY
          MOVE     ZERO      TO   INVALID-FLG
     END-READ.
 SHI-READ-END.
     EXIT.
*****************<<  PROGRAM  END  >>***********************
